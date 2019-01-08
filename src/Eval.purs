module PsLisp.Eval where

import Data.Tuple

import Control.Alt ((<|>))
import Data.List (List(..), foldl, foldr, (:))
import Data.Maybe (Maybe(..))
import Prelude (bind, show, ($), (+), (-), (<$>), (<>))
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
  Tuple "-" (Proc minus)
  ) : Nil

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

eval' :: Expr -> Env -> EvalResult
eval' (Atom s) _ = Ok (Tuple (Atom s) Nil)
-- evaluate a list
eval' (List ((Atom name):xs)) env = do
  let lookedUp = (maybeToResult $ lookup name env) <|> (Error ("Couldnt find \""<>name<>"\" in environment."))
  case lookedUp of
      Ok (Proc f) -> (f xs env)
      Error e -> Error e
      Ok e -> Error ("First arg to list must be a function, can't apply: " <> show e)
eval' (List (x:_)) _ = Error ("First arg to list must be a function, found: " <> show x)
eval' (List (Nil)) _ = Error ("Cannot evaluate empty list")
eval' (Proc _)   _ = Error "Cannot eval procedure"
eval' s    _ = Ok (Tuple s Nil)
