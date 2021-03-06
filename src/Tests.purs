module Test.Main where

import Prelude

import Data.List (List(..), (:))
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (fromEffectFnAff)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import PsLisp (Env, Expr(..), Result(..), EvalResult)
import PsLisp.Eval (evalBlock', stdLib, defineMultipleInEnv)
import PsLisp.Parse (readProgram, readExpr)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (log, print)
import Test.Unit.Main (runTest)

evalResultEqualsExpr :: Result Expr -> Expr -> Boolean
evalResultEqualsExpr (Ok e) expr = e == e
evalResultEqualsExpr _ _ = false

readAndEvalWithLib :: String -> String -> Result Expr
readAndEvalWithLib prelude string = do
  let formatPreludeError :: EvalResult -> EvalResult
      formatPreludeError x = case x of
                                   Error e -> Error $ "Prelude-error: " <> show e
                                   e -> e
  preludeResult <- formatPreludeError $ (readProgram prelude) >>= (flip evalBlock' $ stdLib)
  let fullResult = (readProgram string) >>= (flip evalBlock' $ defineMultipleInEnv (snd preludeResult) stdLib)
  case fullResult of
       Ok (Tuple expr _) -> Ok expr
       Error e           -> Error e

filecontents :: Aff String
filecontents = (FS.readTextFile) UTF8 "./src/prelude.lisp"
readAndEvalAff :: Aff (String -> Result Expr)
readAndEvalAff = readAndEvalWithLib <$> filecontents

main :: Effect Unit
main = runTest do
  suite "parsing" do
    test "Successful" do
      Assert.equal (Ok ((List (Atom("+") : Int 1: Int 2: Int 3: Nil)) : Nil)) (readProgram "(+ 1 2 3)")
      Assert.equal (Ok (Boolean(true) : Nil)) (readProgram "true")
      Assert.equal (Ok ((List (String("+") : Nil)) : Nil)) (readProgram "(\"+\")")
      Assert.equal (Ok (String("hello there") : Nil)) (readProgram "\"hello there\"")
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
       readAndEval <- readAndEvalAff
       Assert.equal (Ok (Int 9)) (readAndEval "(int-plus 1 8)")
       Assert.equal (Ok (Int 5)) (readAndEval "(int-plus (int-plus 1 2) 2)")
       Assert.equal (Ok (Int 10)) (readAndEval "(int-plus (int-plus 1 2) (int-plus 3 4))")
       Assert.equal (Ok (Int 2)) (readAndEval "(int-plus (int-plus 1 2) (int-minus 3 4))")
       Assert.equal (Ok (Int 0)) (readAndEval "(int-minus 3 (int-minus 3 0))")
    test "lambdas" do
       readAndEval <- readAndEvalAff
       Assert.equal (Ok (Int 3)) (readAndEval """((lambda (x) x) 3)""")
       Assert.equal (Ok (Int 3)) (readAndEval "((lambda (x) (int-plus 1 x)) 2)")
       Assert.equal (Error "Expected nr of args: 2 but got: 1") (readAndEval """
                    (define f (lambda (x a . xs)
                    (int-plus x a)))
                    (f 1)
                    """)
       Assert.equal (Ok (Int 5)) (readAndEval """
                    (define f (lambda (x a . xs)
                    (int-plus x a)))
                    (f 2 3)
                    """)
    test "scoping" do
       readAndEval <- readAndEvalAff
       Assert.equal (Ok (Int 1))
                    (readAndEval """
                     (define a 1)
                     (define f (lambda () a))
                     (f)
                    """)
       Assert.equal (Error "Couldn't find \"a\" in environment.")
                    (readAndEval """
                     (define f (
                      lambda ()
                             (define a 5)
                             a))
                     (f)
                     a
                    """)
       Assert.equal (Error "Couldn't find \"y\" in environment.")
                    (readAndEval """
                     (define f (
                      lambda (x y)
                      (f2 x)))
                     (define f2 (
                      lambda (x)
                        (int-plus x y)))
                     (f 0 100)
                    """)
    test "non-dotted lambda errors" do
       readAndEval <- readAndEvalAff
       Assert.equal (Error "Cannot bind 1 to variable") (readAndEval "((lambda (1) (+ 1 x)) 2)")
       Assert.equal (Error "Expected list of args, found: 1") (readAndEval "((lambda 1 (+ 1 x)) 2)")
       Assert.equal (Error "Expected list of args, found: a") (readAndEval "((lambda a (+ 1 x)) 2)")
       Assert.equal (Error "Cannot bind 1 to variable") (readAndEval "((lambda (1 2) (+ 1 x)) 2)")
    test "cons" do
       readAndEval <- readAndEvalAff
       Assert.equal (Ok (List (Int 1 : Nil))) (readAndEval "(cons 1 '())")
       Assert.equal (Ok (List (Int 1 : Int 2 : Int 3 : Nil))) (readAndEval "(cons 1 '(2 3))")
       Assert.equal (Ok (List (Boolean true : Int 2 : Int 3 : Nil))) (readAndEval "(cons true '(2 3))")
    test "car" do
       readAndEval <- readAndEvalAff
       Assert.equal (Ok (Int 5)) (readAndEval "(car '(5 4 2))")
       Assert.equal (Ok (Int 9)) (readAndEval "(car (cons 9 '(2 3)))")
       Assert.equal (Error ("Cannot car empty list")) (readAndEval "(car '())")
    test "cdr" do
       readAndEval <- readAndEvalAff
       Assert.equal (Ok (List (Int 2 : Int 3 : Nil))) (readAndEval "(cdr '(1 2 3))")
       Assert.equal (Ok (List (Int 2 : Int 3 : Nil))) (readAndEval "(cdr (cons 1 '(2 3)))")
       Assert.equal (Error ("Cannot cdr empty list")) (readAndEval "(cdr '())")
    test "lists" do
       readAndEval <- readAndEvalAff
       Assert.equal (Ok (Int 5)) (readAndEval "'5")
       Assert.equal (Ok (List Nil)) (readAndEval "'()")
       Assert.equal (Ok (List (Int 1 : Int 2 : Int 3 : Nil))) (readAndEval "'(1 2 3)")
       Assert.equal (Ok (List ((Boolean true) : Nil))) (readAndEval "'(true)")
    test "define" do
       let readAndEval = readAndEvalWithLib ""
       Assert.equal (Ok Null) (readAndEval "(define a 3)")
       Assert.equal (Ok Null) (readAndEval "(define a (int-plus 2 3))")
       Assert.equal (Ok (Int 10)) (readAndEval """
                    ((lambda (x)
                      (define a (int-plus 2 3))
                      (int-plus a x)) 5)
                    """)
       Assert.equal (Ok (Int 100)) (readAndEval """
                    (define f (lambda (x)
                      (define a 2)
                      (define b 2)
                      (int-plus a (int-plus b (int-plus x 1)))))
                    (f 95)
                    """)
       Assert.equal (Ok (Int 15)) (readAndEval """
                    (define f (lambda (x)
                      (define a 2)
                      (define b 2)
                      (int-plus a (int-plus b (int-plus x 1)))))
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
                          ((< x y) (f (int-plus 1 x) 100))
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
                          (true (fib-iter (int-minus n 1) b (int-plus a b))))))
                    (define fib (lambda (n) (fib-iter n 0 1)))
                    (fib 10)
                    """)
       Assert.equal (Ok (Int 102334155))
                    (readAndEval """
                    (define fib-iter
                      (lambda (n a b)
                        (cond
                          ((= n 0) a)
                          (true (fib-iter (int-minus n 1) b (int-plus a b))))))
                    (define fib (lambda (n) (fib-iter n 0 1)))
                    (fib 40)
                    """)
    test "map" do
       let readAndEval = readAndEvalWithLib ""
       Assert.equal (Ok (Int 5)) (readAndEval """
                    (cond
                      ((nil? 5) xs)
                      (true 5))
                    """)
       Assert.equal (Ok (List (Int 5 : Int 6 : Int 7 : Nil))) (readAndEval """
                    (define map
                      (lambda (f xs)
                        (cond
                          ((nil? xs) xs)
                          (true (cons (f (car xs)) (map f (cdr xs))))
                        )
                    ))
                    (define add-five (lambda (x) (int-plus 5 x)))
                    (map add-five '(0 1 2))
                    """)
       Assert.equal (Ok (List (Int 2 : Int 3 : Int 4 : Nil))) (readAndEval """
                    (define map
                      (lambda (f xs)
                        (cond
                          ((nil? xs) xs)
                          (true (cons (f (car xs)) (map f (cdr xs))))
                        )
                    ))
                    (map (lambda (x) (int-plus x 1)) '(1 2 3))
                    """)
       Assert.equal (Ok (List (Nil))) (readAndEval """
                    (define map
                      (lambda (f xs)
                        (cond
                          ((nil? xs) xs)
                          (true (cons (f (car xs)) (map f (cdr xs))))
                        )
                    ))
                    (map (lambda (x) (int-plus x 1)) '())
                    """)
    test "dotted lists" do
       readAndEval <- readAndEvalAff
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
