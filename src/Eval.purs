module PsLisp.Eval where

import Data.Tuple

import Control.Alt ((<|>))
import Data.List (List(..), foldl, (:))
import Data.Maybe (Maybe(..))
import Prelude (bind, discard, pure, show, ($), (+), (<$>), (<>))
import PsLisp (Env, EvalResult, Expr(AtomE, ListE, IntE, ProcE), Result(..))

plus :: List Expr -> Env -> EvalResult
plus xs env = do
  let evaluatedList = evaluateList xs env
  addEnvToExprResult (foldResultExprList add evaluatedList) env
  where add (Ok (IntE x)) (Ok (IntE y)) = Ok (IntE (x + y))
        add (Ok left)                 e = Error ("Can't add left " <> show left <> " with right " <> show e)
        add e                (Ok right) = Error ("Can't add right " <> show right <> " with left " <> show e)
        add (Error e)                 _ = Error e

foldResultExprList :: (Result Expr -> Result Expr -> Result Expr) -> List (Result Expr) -> Result Expr
foldResultExprList f (x:xs) = foldl f x xs
foldResultExprList f (Nil) = Error "Cannot evaluate empty list"

evaluateList :: List Expr -> Env -> List (Result Expr)
evaluateList xs env = foldl (\acc x -> (fst <$> eval' x env) : acc ) Nil xs
  where evaluateAndGetExpr x = getExprFromEvalResult $ eval' x env

stdLib :: Env
stdLib = (
  Tuple "+" (ProcE plus)
  ) : Nil

getExprFromEvalResult :: EvalResult -> Result Expr
getExprFromEvalResult r = fst <$> r

addEnvToExprResult :: Result Expr -> Env -> EvalResult
addEnvToExprResult re env = addEnv <$> re
  where addEnv e = Tuple e env

maybeToResult (Just x) = Ok x
maybeToResult _ = Error "Lookup failed"

eval :: Expr -> EvalResult
eval e = eval' e stdLib

eval' :: Expr -> Env -> EvalResult
eval' (AtomE s) _ = Ok (Tuple (AtomE s) Nil)
-- evaluate a list
eval' (ListE ((AtomE name):xs)) env = do
  let lookedUp = (maybeToResult $ lookup name env) <|> (Error ("Couldnt find \""<>name<>"\" in environment."))
  case lookedUp of
      Ok (ProcE f) -> (f xs env)
      Error e -> Error e
      Ok e -> Error ("First arg to list must be a function, can't apply: " <> show e)
eval' (ListE (x:_)) _ = Error ("First arg to list must be a function, found: " <> show x)
eval' (ListE (Nil)) _ = Error ("Cannot evaluate empty list")
eval' (ProcE _)   _ = Error "Cannot eval procedure"
eval' s    _ = Ok (Tuple s Nil)
