module Main where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import PsLisp (EvalResult, Result(..))
import PsLisp.Eval (eval)
import PsLisp.Parse (readExpr)

showResult :: EvalResult -> String
showResult (Ok (Tuple e _)) = "Result: " <> show e
showResult (Error e) = "Error: " <> e


main :: Effect Unit
main = do
  log "Parsing"
  log (show $ readExpr "(+ 1 1 2)")
  log (show $ readExpr "(+ 1 2 3)" >>= eval)
  log "Evaulating"
  log (showResult $ readExpr "(a + 1)" >>= eval)
  log (showResult $ readExpr "(1 1 1)" >>= eval)
  log (showResult $ readExpr "(+ 1 1)" >>= eval)
  log (showResult $ readExpr "(+ (+ 2 3) (+ 4 5))" >>= eval)
  log (showResult $ readExpr "(+ (+ a 3) (+ 4 5))" >>= eval)
  log (showResult $ readExpr "(- 3 2 1)" >>= eval)
