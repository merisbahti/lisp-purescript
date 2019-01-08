module Test.Main where

import Prelude

import Data.List (List(..), (:))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import PsLisp (Expr(..), Result(..))
import PsLisp.Eval (evalBlock)
import PsLisp.Parse (readProgram, readExpr)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

evalResultEqualsExpr :: Result Expr -> Expr -> Boolean
evalResultEqualsExpr (Ok e) expr = e == e
evalResultEqualsExpr _ _ = false

readAndEval :: String -> Result Expr
readAndEval string = do
  let fullResult = readProgram string >>= evalBlock
  case fullResult of
       Ok (Tuple expr _) -> Ok expr
       Error e           -> Error e

main :: Effect Unit
main = runTest do
  suite "parsing" do
    test "Successful" do
      Assert.equal (readProgram "(+ 1 2 3)") (Ok ((List (Atom("+") : Int 1: Int 2: Int 3: Nil)) : Nil))
      Assert.equal (readProgram "(+ 1 (+ 2 3) 44 5)") (Ok ((
                   List (Atom("+") : Int(1) : (
                   List (Atom("+") : Int(2) : Int (3) : Nil)
                   ) : Int(44) : Int (5) : Nil)) : Nil))
    test "Failing " do
      Assert.equal (readExpr "(+ 1 2 3") (Error "(ParseError \"Expected ')'\" (Position { line: 1, column: 9 }))")
  suite "evaluating" do
    test "Eval arithmetic" do
       Assert.equal (readAndEval "(+ 1 2)") (Ok (Int 3))
       Assert.equal (readAndEval "(+ (+ 1 2) 2)") (Ok (Int 5))
       Assert.equal (readAndEval "(+ (+ 1 2) (+ 3 4))") (Ok (Int 10))
       Assert.equal (readAndEval "(+ (+ 1 2) (- 3 4))") (Ok (Int 2))
       Assert.equal (readAndEval "(- 3 2 1)") (Ok (Int 0))
    test "lambdas" do
       Assert.equal (readAndEval "((lambda (x) x) 3)") (Ok (Int 3))
       Assert.equal (readAndEval "((lambda (x) (+ 1 x)) 2)") (Ok (Int 3))
    test "define" do
       Assert.equal (readAndEval "(define a 3)") (Ok Null)
       Assert.equal (readAndEval "(define a (+ 2 3))") (Ok Null)
       Assert.equal (readAndEval """
                    ((lambda (x)
                      (define a (+ 2 3))
                      (+ a x)) 5)
                    """) (Ok (Int 10))
       Assert.equal (readAndEval """
                    (define f (lambda (x)
                      (define a 2)
                      (define b 2)
                      (+ a b x 1)))
                    (f 10)
                    """) (Ok (Int 15))
