module PsLisp.Parse where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.List (List, many, (:))
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import PsLisp (Expr(..), Result(..))
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (sepBy, try)
import Text.Parsing.Parser.String (char, oneOf, whiteSpace)

type SParser a = Parser String a

symbol :: SParser Char
symbol = oneOf $ toCharArray "!#$%&|*+-/:<=>?@^_~"

digit :: SParser Char
digit = oneOf $ toCharArray "0123456789"

letter :: SParser Char
letter = oneOf $ toCharArray "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

charlistToString :: List Char -> String
charlistToString = fromCharArray <<< fromFoldable

parseAtom :: SParser Expr
parseAtom = do
  first <- letter <|> symbol
  rest <- many $ letter <|> symbol <|> digit
  let atom = charlistToString $ first:rest
  pure $ case atom of
              "true" -> Boolean true
              "false" -> Boolean false
              _ -> Atom atom

parseInt :: SParser Expr
parseInt = do
  first <- digit
  rest <- many $ digit
  let parsedDigits = (\x -> fromMaybe 0 (fromString x)) <<< charlistToString $ first:rest
  pure $ Int (parsedDigits)

parseList :: SParser Expr -> SParser Expr
parseList pars = List <$> pars `sepBy` whiteSpace

parseDottedList :: SParser Expr
parseDottedList = do
   init <- many $ whiteSpace *> parseAtom <* whiteSpace
   _ <- whiteSpace *> char '.' <* whiteSpace
   rest <- whiteSpace *> parseAtom <* whiteSpace
   pure $ DottedList init rest

parseExpr :: SParser Expr
parseExpr = fix $ \p -> whiteSpace *> (parseInt
                     <|> (do
                         _ <- char '('
                         x <- (try parseDottedList <|> try (parseList p))
                         _ <- char ')'
                         pure x)
                     <|> parseAtom
) <* whiteSpace

readExpr :: String -> Result Expr
readExpr input = case runParser input parseExpr of
  Left err -> Error (show err)
  Right value -> Ok value

readProgram :: String -> Result (List Expr)
readProgram input = case runParser input (many $ parseExpr) of
  Left err -> Error (show err)
  Right value -> Ok value
