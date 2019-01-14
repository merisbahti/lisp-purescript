module PsLisp.Eval where

import Data.Tuple

import Control.Alt ((<|>))
import Control.MonadPlus (guard)
import Data.List (List(..), filter, foldl, foldr, head, length, slice, tail, zip, (:))
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Prelude (bind, map, pure, show, ($), (+), (-), (/=), (<), (<$>), (<=), (<>), (==))
import PsLisp (Env, EvalResult, Expr(..), Result(..))


stdLib :: Env
stdLib = (
  Tuple "int-plus" (Proc plus)
  ):(
  Tuple "lambda" (SpecialForm lambda)
  ):(
  Tuple "int-minus" (Proc minus)
  ):(
  Tuple "define" (SpecialForm define)
  ):(
  Tuple "cond" (SpecialForm cond)
  ):(
  Tuple "<" (Proc lt)
  ):(
  Tuple "=" (Proc eq)
  ):(
  Tuple "cons" (Proc cons)
  ):(
  Tuple "car" (Proc car)
  ):(
  Tuple "cdr" (Proc cdr)
  ):(
  Tuple "nil?" (Proc isNil)
  ) : Nil

plus :: List Expr -> Env -> EvalResult
plus = makeBinOp op
  where op (Int x) (Int y)  = Ok $ Int (x + y)
        op x       y        = Error ("Can't plus value: " <> show x <> " with value " <> show y)

minus :: List Expr -> Env -> EvalResult
minus = makeBinOp op
  where op (Int x) (Int y)  = Ok $ Int (x - y)
        op x       y        = Error ("Can't minus value: " <> show x <> " with value " <> show y)

makeBinOp :: (Expr -> Expr -> Result Expr) -> List Expr -> Env -> EvalResult
makeBinOp op (lh:rh:Nil) env = do
  addEnvToExprResult (op lh rh) env
makeBinOp _ _ _ = Error "More than one arg passed to binary operator"

makeUnaryOp :: (Expr -> Result Expr) -> List Expr -> Env -> EvalResult
makeUnaryOp op (x : Nil) env = do
  addEnvToExprResult (op x) env
makeUnaryOp _ _ _ = do
  Error ("Unary operator takes 1 argument")

foldResultExprList :: (Result Expr -> Result Expr -> Result Expr) -> List (Result Expr) -> Result Expr
foldResultExprList f (x:xs) = foldr f x xs
foldResultExprList f (Nil) = Error "Cannot evaluate empty list"

evaluateList :: List Expr -> Env -> List (Result Expr)
evaluateList xs env = foldl (\acc x -> (fst <$> eval' x env) : acc ) Nil xs

evaluateAndGetExpr :: Expr -> Env -> Result Expr
evaluateAndGetExpr x env = getExprFromEvalResult $ eval' x env

cons :: List Expr -> Env -> EvalResult
cons = makeBinOp op
  where op x (List xs) = Ok $ List (x : xs)
        op a b         = Error ("Cannot cons " <> show a <> " to " <> show b)

car :: List Expr -> Env -> EvalResult
car = makeUnaryOp op
  where op (List xs) = do
          elem <- (maybeToResult $ head xs) <|> Error "Cannot car empty list"
          pure $ elem
        op x         = Error $ "car can only be on lists, got:" <> show x

cdr :: List Expr -> Env -> EvalResult
cdr = makeUnaryOp op
  where op (List xs) = do
          elem <- (maybeToResult $ tail xs) <|> Error "Cannot cdr empty list"
          pure $ List (elem)
        op e         = Error $ "Cannot cdr " <> show e

isNil :: List Expr -> Env -> EvalResult
isNil = makeUnaryOp op
  where op (List Nil) = Ok $ Boolean true
        op _          = Ok $ Boolean false

lt :: List Expr -> Env -> EvalResult
lt = makeBinOp op
  where op (Int a) (Int b) = Ok $ Boolean (a < b)
        op a b             = Error ("Cannot compare " <> show a <> " to " <> show b)

eq :: List Expr -> Env -> EvalResult
eq = makeBinOp op
  where op (Int a) (Int b) = Ok $ Boolean (a == b)
        op a b             = Error ("Cannot compare " <> show a <> " to " <> show b)

evalEnvRemoved :: Expr -> Env -> Result Expr
evalEnvRemoved expr env = do
  res <- eval' expr env
  pure $ fst res

cond :: List Expr -> Env -> EvalResult
cond exprs env = do
  condPairs <- getCondPairs exprs
  cond' $ condPairs
  where
        cond' :: List (Tuple Expr Expr) -> EvalResult
        cond' tuples = case foldl reducer (Ok Nothing) tuples of
               Ok (Just e)  -> eval' e env
               Ok (Nothing) -> Ok (Tuple (Null) (env))
               Error e      -> Error e
          where reducer :: Result (Maybe Expr) -> (Tuple Expr Expr) -> Result (Maybe Expr)
                reducer (Ok (Just a))                  _  = Ok (Just a)
                reducer (Ok (Nothing)) (Tuple pred consequent)  = do
                   evaledPred <- evalEnvRemoved pred env
                   pure $ case evaledPred of
                        Boolean true -> Just consequent
                        _  -> Nothing
                reducer a                               _ = a
        getCondPairs :: List Expr -> Result (List (Tuple Expr Expr))
        getCondPairs pairs = do
           consequent <- sequence $ getCondPair <$> pairs
           pure consequent
        getCondPair :: Expr -> Result (Tuple Expr Expr)
        getCondPair (List(a:b:Nil)) = Ok (Tuple a b)
        getCondPair (e)          = Error ("Cond expects pair of two expressions, found: " <> show e)

define :: List Expr -> Env -> EvalResult
define (Atom(a):b:_) env = do
  evaluated <- eval' b env
  pure $ Tuple Null (defineInEnv (Tuple a (fst evaluated)) env)
define (Atom(_):_) _ = Error ("Second arg to define missing, cannot define.")
define _ _ = Error ("Define takes two args: variable to be bound and expression.")


lambda :: List Expr -> Env -> EvalResult
lambda exprs freeVars = do
  args <- maybeToResult $ head exprs
  block <- maybeToResult $ tail exprs
  case args of
       DottedList xs rest -> do
          xsParsed <- getVarAtoms xs
          restParsed <- getStringFromAtom rest
          pure $ Tuple (Proc (newDottedListProc (Tuple xsParsed restParsed) block)) freeVars
       List xs -> do
          varNames <- getVarAtoms xs
          pure $ Tuple (Proc (newProc varNames block)) freeVars
       e -> Error ("Expected list of args, found: " <> show e)
     where newProc :: List String -> List Expr -> List Expr -> Env -> EvalResult
           newProc varNames body boundExprs procEnv = do
             let blen = length boundExprs
             let vlen = length varNames
             _ <- maybeToResult (guard $ vlen == blen ) <|> Error ("Expected nr of args: " <> show vlen <> " but got: " <> show blen)
             let zippedArgs = zip varNames boundExprs
             procEvalResult <- evalBlock' body (defineMultipleInEnv zippedArgs procEnv)
             pure $ Tuple (fst procEvalResult) freeVars
           getVarAtoms :: List Expr -> Result (List String)
           getVarAtoms (x:xs) = do
              atom <- (getStringFromAtom x)
              rest <- getVarAtoms xs
              pure $ atom : rest
           getVarAtoms (Nil)  = Ok (Nil)
           getStringFromAtom :: Expr -> Result String
           getStringFromAtom (Atom x) = pure x
           getStringFromAtom e        = Error ("Cannot bind " <> show e <> " to variable")
           newDottedListProc :: Tuple (List String) String -> List Expr -> List Expr -> Env -> EvalResult
           newDottedListProc (Tuple varNames restVars) body boundExprs env = do
              let blen = length boundExprs
              let vlen = length varNames
              _ <- maybeToResult (guard $ vlen <= blen ) <|> Error ("Expected nr of args: " <> show vlen <> " but got: " <> show blen)
              let zippedInitArgs :: List (Tuple String Expr)
                  zippedInitArgs = zip varNames boundExprs
              let restArgs :: Tuple String Expr
                  restArgs = Tuple restVars (List (slice (length zippedInitArgs) (length boundExprs) boundExprs))
              procEvalResult <- evalBlock' body (defineMultipleInEnv (restArgs : zippedInitArgs) env)
              pure $ Tuple (fst procEvalResult) freeVars


evaluateListOfExpressions :: List Expr -> Env -> Result (List Expr)
evaluateListOfExpressions exprList env = do
   sequence $ map (\x -> evalEnvRemoved x env) exprList


lookupEnv :: String -> Env -> Result Expr
lookupEnv name env = (maybeToResult $ lookup name env) <|> (Error ("Couldnt find \""<>name<>"\" in environment:" <> show env))

defineInEnv :: (Tuple String Expr) -> Env -> Env
defineInEnv e@(Tuple name expr) env = e : (filter (\x -> (fst x) /= name) env)

defineMultipleInEnv :: Env -> Env -> Env
defineMultipleInEnv newVars env = foldr defineInEnv env newVars

getExprFromEvalResult :: EvalResult -> Result Expr
getExprFromEvalResult r = fst <$> r

addEnvToExprResult :: Result Expr -> Env -> EvalResult
addEnvToExprResult re env = addEnv <$> re
  where addEnv e = Tuple e env

maybeToResult :: ∀ t4. Maybe t4 → Result t4
maybeToResult (Just x) = Ok x
maybeToResult _ = Error "Lookup failed"

eval :: Expr -> EvalResult
eval e = eval' e stdLib

evalBlock' :: List Expr -> Env -> EvalResult
evalBlock' exprs startEnv = foldl step (Ok (Tuple Null startEnv )) exprs
  where step :: EvalResult -> Expr -> EvalResult
        step res expr = do
           tuple <- res
           let newEnv :: Env
               newEnv = snd tuple
           eval' expr newEnv

eval' :: Expr -> Env -> EvalResult
eval' (Atom s) env = do
  value <- lookupEnv s env
  pure $ Tuple value env
eval' (Quoted expr) env = do
  pure $ Tuple expr env
eval' (List (x:xs)) env = do
  evaled <- eval' x env
  case evaled of
       Tuple (Proc f) newEnv -> do
          evaledArgs <- evaluateListOfExpressions xs env
          f evaledArgs env
       Tuple (SpecialForm f) newEnv -> do
          f xs env
       expr   -> Error ("Can't evaluate using: " <> show expr)
eval' (List (Nil)) _ = Error ("Cannot evaluate empty list")
eval' s    e = Ok (Tuple s e)
