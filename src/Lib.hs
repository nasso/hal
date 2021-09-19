module Lib
  ( sEval,
    EvalError (..),
    Context,
    newContext,
  )
where

import Control.Applicative (Alternative (many))
import Grammar.Datum (Datum, datum)
import Parsing (Parser (parse))

newtype Context
  = Context ()
  deriving (Eq, Show)

data EvalError
  = SyntaxError
  | Extra [Datum] String
  deriving (Show, Eq)

newContext :: Context
newContext = Context ()

sEval :: Context -> String -> Either EvalError (Maybe Datum, Context)
sEval ctx src = case parse (many datum) src of
  Nothing -> Left SyntaxError
  Just (v, "") -> Right $ foldl (\(_, c) d -> dEval c d) (Nothing, ctx) v
  Just (v, e) -> Left $ Extra v e

dEval :: Context -> Datum -> (Maybe Datum, Context)
dEval c d = (Just d, c)
