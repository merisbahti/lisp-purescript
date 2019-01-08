module PsLisp.Eval where

import Data.Tuple

import Control.Alt ((<|>))
import Data.List (List(..), filter, foldl, foldr, head, tail, zip, (:))
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Prelude (bind, map, pure, show, ($), (+), (-), (<$>), (<>), (==), (>>=))
import PsLisp (Env, EvalResult, Expr(..), Result(..))

plus :: List Expr -> Env -> EvalResult
plus = makeOperator op
  where op (Ok (Int x)) (Ok (Int y)) = Ok (Int (x + y))
        op (Error e)                 _ = Error (e)
        op _                 (Error e) = Error (e)
        op (Ok x)               (Ok y) = Error ("Can't add value: " <> show x <> " with value " <> show y)

minus :: List Expr -> Env -> EvalResult
minus = makeOperator op
  where op (Ok (Int x)) (Ok (Int y)) = Ok (Int (x - y))
        op (Error e)                 _ = Error (e)
        op _                 (Error e) = Error (e)
        op (Ok x)               (Ok y) = Error ("Can't minus value: " <> show x <> " with value " <> show y)

makeOperator :: (Result Expr -> Result Expr -> Result Expr) -> List Expr -> Env -> EvalResult
makeOperator operator xs env = do
  let evaluatedList = evaluateList xs env
  addEnvToExprResult (foldResultExprList operator evaluatedList) env

foldResultExprList :: (Result Expr -> Result Expr -> Result Expr) -> List (Result Expr) -> Result Expr
foldResultExprList f (x:xs) = foldr f x xs
foldResultExprList f (Nil) = Error "Cannot evaluate empty list"

evaluateList :: List Expr -> Env -> List (Result Expr)
evaluateList xs env = foldl (\acc x -> (fst <$> eval' x env) : acc ) Nil xs
  where evaluateAndGetExpr x = getExprFromEvalResult $ eval' x env

stdLib :: Env
stdLib = (
  Tuple "+" (Proc plus)
  ):(
  Tuple "lambda" (Proc lambda)
  ):(
  Tuple "-" (Proc minus)
  ):(
  Tuple "define" (Proc define)
  ) : Nil

define :: List Expr -> Env -> EvalResult
define (Atom(a):b:_) env = do
  evaluated <- eval' b env
  pure $ Tuple Null (defineInEnv (Tuple a (fst evaluated)) env)
define (Atom(_):_) _ = Error ("Second arg to define missing, cannot define.")
define _ _ = Error ("Define takes two args: variable to be bound and expression.")


lambda :: List Expr -> Env -> EvalResult
lambda exprs freeVars = do
  let args :: Result Expr
      args = maybeToResult $ head exprs
  block <- maybeToResult $ tail exprs
  varNames <- getVarNames <$> args
  -- TODO: assert that lambda takes 2 arguments for now.
  case varNames of
       Ok (e) -> Ok (Tuple (Proc (newProc e block)) freeVars)
       Error (e) -> Error e
     where newProc :: List String -> List Expr -> List Expr -> Env -> EvalResult
           newProc varNames body boundExprs env = do
             evaluatedBoundExprs <- evaluateListOfExpressions boundExprs env
             let zippedArgs :: List (Tuple String Expr)
                 zippedArgs = zip varNames evaluatedBoundExprs
             procEvalResult <- evalBlock body (defineMultipleInEnv zippedArgs env)
             pure $ case procEvalResult of
                      Tuple procExpr _ -> Tuple procExpr env
           getVarNames :: Expr -> Result (List String)
           getVarNames (List(Atom(x):xs)) = do
              rest <- getVarNames(List(xs))
              pure (x : rest)
           getVarNames (List(Nil)) = Ok (Nil)
           getVarNames (List(x:xs)) = Error ("\"" <> show x <> "\"" <> "Cannot be bound to a variable")
           getVarNames (x) = Error ("Variable list must be list, found: \"" <> show x)

           evaluateListOfExpressions :: List Expr -> Env -> Result (List Expr)
           evaluateListOfExpressions exprList env = do
               sequence $ map evalAndGetExpr exprList
                  where evalAndGetExpr :: Expr -> Result Expr
                        evalAndGetExpr expr = fst <$> (eval' expr env)


lookupEnv :: String -> Env -> Result Expr
lookupEnv name env = (maybeToResult $ lookup name env) <|> (Error ("Couldnt find \""<>name<>"\" in environment:" <> show env))

defineInEnv :: (Tuple String Expr) -> Env -> Env
defineInEnv e@(Tuple name expr) env = e : env

defineMultipleInEnv :: List (Tuple String Expr) -> Env -> Env
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

evalBlock :: List Expr -> Env -> EvalResult
evalBlock exprs startEnv = foldl step (Ok (Tuple Null startEnv )) exprs
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
-- evaluate a list
eval' (List ((Atom name):xs)) env = do
  let lookedUp = lookupEnv name env
  case lookedUp of
      Ok (Proc f) -> (f xs env)
      Error e -> Error e
      Ok e -> Error ("First arg to list must be a function, can't apply: " <> show e)
eval' (List (x:xs)) env = do
  let evaled :: EvalResult
      evaled = eval' x env
  case evaled of
       Ok (Tuple (Proc f) _) -> f xs env
       Ok (Tuple expr _) -> Error ("Can't evaluate using: " <> show expr)
       Error e -> Error e
eval' (List (Nil)) _ = Error ("Cannot evaluate empty list")
eval' (Proc _)   _ = Error "Cannot eval procedure"
eval' s    _ = Ok (Tuple s Nil)
