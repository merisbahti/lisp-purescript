module PsLisp where

import Data.List

import Control.Alternative (class Alt)
import Data.Tuple (Tuple)
import Prelude (class Applicative, class Apply, class Bind, class Eq, class Functor, class Show, map, show, ($), (<$>), (<<<), (<>), (==))

instance showExpr :: Show Expr where
  show (Atom s) = s
  show (List xs) = "(" <> (intercalate " " <<< map show $ xs) <> ")"
  show (Int i) = show i
  show (Boolean b) = show b
  show (Proc _) = "Procedure"
  show (String s) = "\""<> s <>"\""
  show (Quoted e) = "'" <> show e
  show (DottedList init rest) = "(" <> (intercalate " " <<< map show $ init) <> " . " <> show rest <> ")"
  show Null = "Null"

type EvalResult = Result (Tuple Expr Env)

type Env = List (Tuple String Expr)

data Expr
  = Atom String
  | List (List Expr)
  | Int Int
  | Proc (List Expr -> Env -> EvalResult)
  | String String
  | DottedList (List Expr) Expr
  | Boolean (Boolean)
  | Quoted (Expr)
  | Null

data Result a = Ok a
              | Error String

instance eqExpr :: Eq Expr where
  eq (Int a)        (Int b)     = a == b
  eq (Atom a)       (Atom b)    = a == b
  eq (Boolean a)    (Boolean b) = a == b
  eq (Null)         (Null)      = true
  eq (List a)       (List b)    = a == b
  eq (String a)     (String b)  = a == b
  eq _              _           = false

instance eqResult :: Eq a => Eq (Result a) where
  eq (Ok a)       (Ok b) = a == b
  eq (Error a) (Error b) = a == b
  eq _ _                 = false

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
