module PsLisp where

import Data.List

import Control.Alternative (class Alt)
import Data.Tuple (Tuple)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Show, show, (<$>), (<>))

instance showExpr :: Show Expr where
  show (Atom s) = s
  show (List (x:xs)) = "(" <> foldl (\acc expr ->  acc <> " " <> (show expr)) (show x) xs <> ")"
  show (List (nil)) = "()"
  show (Int i) = show i
  show (Proc _) = "Procedure"
  show (String s) = "\""<> s <>"\""

type EvalResult = Result (Tuple Expr Env)

type Env = List (Tuple String Expr)

data Expr
  = Atom String
  | List (List Expr)
  | Int Int
  | Proc (List Expr -> Env -> EvalResult)
  | String String

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
