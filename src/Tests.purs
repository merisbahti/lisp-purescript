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
      Assert.equal (Ok ((List (Atom("+") : Int 1: Int 2: Int 3: Nil)) : Nil)) (readProgram "(+ 1 2 3)")
      Assert.equal (Ok (Boolean(true) : Nil)) (readProgram "true")
      Assert.equal (Ok (Boolean(false) : Nil)) (readProgram "false")
      Assert.equal (Ok ((
                   List (Atom("+") : Int(1) : (
                   List (Atom("+") : Int(2) : Int (3) : Nil)
                   ) : Int(44) : Int (5) : Nil)) : Nil))
                  (readProgram "(+ 1 (+ 2 3) 44 5)")
    test "Failing " do
      Assert.equal (readExpr "(+ 1 2 3") (Error "(ParseError \"Expected ')'\" (Position { line: 1, column: 9 }))")
  suite "evaluating" do
    test "Eval arithmetic" do
       Assert.equal (Ok (Int 3)) (readAndEval "(+ 1 2)")
       Assert.equal (Ok (Int 5)) (readAndEval "(+ (+ 1 2) 2)")
       Assert.equal (Ok (Int 10)) (readAndEval "(+ (+ 1 2) (+ 3 4))")
       Assert.equal (Ok (Int 2)) (readAndEval "(+ (+ 1 2) (- 3 4))")
       Assert.equal (Ok (Int 0)) (readAndEval "(- 3 2 1)")
    test "lambdas" do
       Assert.equal (Ok (Int 3)) (readAndEval "((lambda (x) x) 3)")
       Assert.equal (Ok (Int 3)) (readAndEval "((lambda (x) (+ 1 x)) 2)")
    test "non-dotted lambda errors" do
       Assert.equal (Error "\"1\" cannot be bound to a variable") (readAndEval "((lambda (1) (+ 1 x)) 2)")
       Assert.equal (Error "Variable list must be list of atoms, found: \"1\"") (readAndEval "((lambda 1 (+ 1 x)) 2)")
       Assert.equal (Error "Variable list must be list of atoms, found: \"a\"") (readAndEval "((lambda a (+ 1 x)) 2)")
    test "cons" do
       Assert.equal (Ok (List (Int 1 : Nil))) (readAndEval "(cons 1 '())")
       Assert.equal (Ok (List (Int 1 : Int 2 : Int 3 : Nil))) (readAndEval "(cons 1 '(2 3))")
       Assert.equal (Ok (List (Boolean true : Int 2 : Int 3 : Nil))) (readAndEval "(cons true '(2 3))")
    test "car" do
       Assert.equal (Ok (Int 1)) (readAndEval "(car '(1 2 3))")
       Assert.equal (Ok (Int 1)) (readAndEval "(car (cons 1 '(2 3)))")
       Assert.equal (Error ("Cannot car empty list")) (readAndEval "(car '())")
    test "cdr" do
       Assert.equal (Ok (List (Int 2 : Int 3 : Nil))) (readAndEval "(cdr '(1 2 3))")
       Assert.equal (Ok (List (Int 2 : Int 3 : Nil))) (readAndEval "(cdr (cons 1 '(2 3)))")
       Assert.equal (Error ("Cannot cdr empty list")) (readAndEval "(cdr '())")
    test "lists" do
       Assert.equal (Ok (Int 5)) (readAndEval "'5")
       Assert.equal (Ok (List Nil)) (readAndEval "'()")
       Assert.equal (Ok (List (Int 1 : Int 2 : Int 3 : Nil))) (readAndEval "'(1 2 3)")
       Assert.equal (Ok (List ((Boolean true) : Nil))) (readAndEval "'(true)")
    test "define" do
       Assert.equal (Ok Null) (readAndEval "(define a 3)")
       Assert.equal (Ok Null) (readAndEval "(define a (+ 2 3))")
       Assert.equal (Ok (Int 10)) (readAndEval """
                    ((lambda (x)
                      (define a (+ 2 3))
                      (+ a x)) 5)
                    """)
       Assert.equal (Ok (Int 15)) (readAndEval """
                    (define f (lambda (x)
                      (define a 2)
                      (define b 2)
                      (+ a b x 1)))
                    (f 10)
                    """)
       Assert.equal (Ok (Int 15)) (readAndEval """
                    (define f (lambda (x)
                      (define a 2)
                      (define b 2)
                      (+ a b x 1)))
                    (f 10)
                    """)
       Assert.equal (Ok (Int 2))
                    (readAndEval """
                    (define f
                      (lambda (x)
                        (cond
                          (false 0)
                          (true 2)
                          (true 1)
                        )
                      ))
                    (f 1)
                    """)
       Assert.equal (Ok (Boolean true))
                    (readAndEval """
                    (= 5 5)
                    """)
       Assert.equal (Ok (Boolean true))
                    (readAndEval """
                    (define f (lambda (x y) (= 5 x)))
                    (f 5 4)
                    """)
       Assert.equal (Ok (Int 1))
                    (readAndEval """
                    (define f
                      (lambda (x y)
                        (cond
                          ((= 5 x) 1)
                          ((= x 4) 2)
                          (true 3)
                        )
                      ))
                    (f 5 4)
                    """)
       Assert.equal (Ok (Boolean false))
                    (readAndEval """
                    (< 3 2)
                    """)
       Assert.equal (Ok (Boolean false))
                    (readAndEval """
                    (< 3 3)
                    """)
       Assert.equal (Ok (Boolean true))
                    (readAndEval """
                    (< 2 3)
                    """)
       Assert.equal (Ok (Int 50))
                    (readAndEval """
                    (define f
                      (lambda (x)
                        (cond
                          ((< x 10) (f (+ 1 x)))
                          (true x)
                        )
                      ))
                    (f 50)
                    """)
       Assert.equal (Ok (Int 100))
                    (readAndEval """
                    (define f
                      (lambda (x y)
                        (cond
                          ((< x y) (f (+ 1 x)))
                          (true x)
                        )
                      ))
                    (f 0 100)
                    """)
       Assert.equal (Ok (Int 55))
                    (readAndEval """
                    (define fib-iter
                      (lambda (n a b)
                        (cond
                          ((= n 0) a)
                          (true (fib-iter (- n 1) b (+ a b))))))
                    (define fib (lambda (n) (fib-iter n 0 1)))
                    (fib 10)
                    """)
       Assert.equal (Ok (Int 102334155))
                    (readAndEval """
                    (define fib-iter
                      (lambda (n a b)
                        (cond
                          ((= n 0) a)
                          (true (fib-iter (- n 1) b (+ a b))))))
                    (define fib (lambda (n) (fib-iter n 0 1)))
                    (fib 40)
                    """)
    test "map" do
       Assert.equal (Ok (List (Int 2 : Int 3 : Int 4 : Nil))) (readAndEval """
                    (define map
                      (lambda (f xs)
                        (cond
                          ((nil? xs) xs)
                          (true (cons (f (car xs)) (map f (cdr xs))))
                        )
                    ))
                    (map (lambda (x) (+ x 1)) '(1 2 3))
                    """)

       Assert.equal (Ok (List (Nil))) (readAndEval """
                    (define map
                      (lambda (f xs)
                        (cond
                          ((nil? xs) xs)
                          (true (cons (f (car xs)) (map f (cdr xs))))
                        )
                    ))
                    (map (lambda (x) (+ x 1)) '())
                    """)
    test "dotted lists" do
       Assert.equal (Ok (List (Int 1 : Int 2 : Int 3 : Nil))) (readAndEval """
                    (define lizt
                      (lambda (x . xs)
                          (cons x xs)))
                    (lizt 1 2 3)
                    """)
       Assert.equal (Ok (Int (-1))) (readAndEval """
                    (define minus
                      (lambda (x y . xs)
                          (- x y)))
                    (minus 2 3)
                    """)
       Assert.equal (Error "Can't minus value: 2 with value (3)") (readAndEval """
                    (define minus
                      (lambda (x . xs)
                          (- x xs)))
                    (minus 2 3)
                    """)
