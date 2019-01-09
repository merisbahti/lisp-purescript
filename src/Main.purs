module Main where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import PsLisp (EvalResult, Result(..))
import PsLisp.Eval (eval)
import PsLisp.Parse (readExpr)

showResultExpr :: EvalResult -> String
showResultExpr (Ok (Tuple e _)) = "Result: " <> show e
showResultExpr (Error e) = "Error: " <> e

showResult :: EvalResult -> String
showResult (Ok (Tuple e env)) = "Result: " <> show e <> " with env: " <> show env
showResult (Error e) = "Error: " <> e

main :: Effect Unit
main = do
  log "Parsing"
  log (show $ readExpr "(+ 1 1 2)")
  log (show $ readExpr "(+ 1 2 3)")
  log (show $ readExpr "(a ())")
  log (show $ readExpr "(a b . c)")
  log (show $ readExpr """(a b . n)""")
  log "Evaulating"
  log (showResult $ readExpr "(a + 1)" >>= eval)
  log (showResult $ readExpr "(1 1 1)" >>= eval)
  log (showResult $ readExpr "(+ 1 1)" >>= eval)
  log (showResult $ readExpr "(+ (+ 2 3) (+ 4 5))" >>= eval)
  log (showResult $ readExpr "(+ (+ a 3) (+ 4 5))" >>= eval)
  log (showResult $ readExpr "(- 3 2 1)" >>= eval)
  log (showResult $ readExpr "((lambda (x y) (+ x y)) 3 3)" >>= eval)
  log (showResult $ readExpr "((lambda (x y) (+ 3 x)) 5)" >>= eval)
  log (showResult $ readExpr "((lambda (x) x) 3)" >>= eval)
