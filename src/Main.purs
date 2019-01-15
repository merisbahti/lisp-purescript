module Main where

import Prelude

import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile) as FS
import PsLisp (EvalResult, Expr, Result(..))
import PsLisp.Eval (evalBlock', stdLib, defineMultipleInEnv)
import PsLisp.Parse (readProgram)

foreign import args :: Unit -> Array String

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

main :: Effect Unit
main = do
  let myArgs = args unit
  let arg = myArgs !! 2
  case arg of
       Just x -> do
          argFile <- (FS.readTextFile) UTF8 x
          read <- readAndEvalWithLib <$> ((FS.readTextFile) UTF8 "./src/prelude.lisp")
          let _ = read argFile
          log ""
       _ -> log "1 argument: file name"
