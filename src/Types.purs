module PsLisp where

import Data.List

import Control.Alternative (class Alt)
import Data.Tuple (Tuple)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Show, show, (<$>), (<>))

instance showExpr :: Show Expr where
  show (AtomE s) = s
  show (ListE (x:xs)) = "(" <> foldl (\acc expr ->  acc <> " " <> (show expr)) (show x) xs <> ")"
  show (ListE (nil)) = "()"
  show (IntE i) = show i
  show (ProcE _) = "Procedure"

type EvalResult = Result (Tuple Expr Env)

type Env = List (Tuple String Expr)

data Expr
  = AtomE String
  | ListE (List Expr)
  | IntE Int
  | ProcE (List Expr -> Env -> EvalResult)

data Result a = Ok a
              | Error String

instance showResult :: Show a => Show (Result a) where
  show (Ok a) = "Success: " <> show a
  show (Error s) = "Failure: " <> s

instance functorResult :: Functor Result where
  map f (Ok y) = Ok (f y)
  map _ (Error x) = Error x

instance applyResult :: Apply Result where
  apply (Ok f) r = f <$> r
  apply (Error e) _ = Error e

instance altResult :: Alt Result where
  alt (Error _) a =  a
  alt a         _ =  a

instance bindResult :: Bind Result where
  bind (Ok a) f = f a
  bind (Error e)      _ = Error e

instance applicativeResult :: Applicative Result where
  pure a = Ok a
