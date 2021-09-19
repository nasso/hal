module Lib
  ( eval,
    EvalError (..),
  )
where

import Control.Applicative (Alternative (many))
import Grammar.Datum (Datum, datum)
import Parsing (Parser (parse))

data EvalError
  = SyntaxError
  | Extra Datum String
  deriving (Show, Eq)

eval :: String -> Either EvalError Datum
eval src = case parse (many datum) src of
  Just (v, "") -> Right $ last v
  Nothing -> Left SyntaxError
  Just (v, e) -> Left $ Extra (last v) e
