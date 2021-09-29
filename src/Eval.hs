module Eval
  ( newContext,
    program,
    form,
    eval,
    syntaxError,
    raise,
    SyntaxError (..),
    EvalResult (..),
    Eval,
    Context,
    Value (..),
  )
where

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Grammar (Datum (..))

data Formals = Exact [String] | Variadic [String] String deriving (Show, Eq)

data Value = Datum Datum | Procedure Formals [Datum] deriving (Eq)

instance Show Value where
  show (Datum d) = show d
  show (Procedure _ _) = "#<procedure>"

type Context = Map String Value

newtype SyntaxError = SyntaxError String deriving (Show, Eq)

data EvalResult a = Ok a | Exception Datum deriving (Show, Eq)

newContext :: Context
newContext = Map.empty

newtype Eval a = Eval
  { eval ::
      [Datum] ->
      Context ->
      Either SyntaxError (EvalResult a, Context, [Datum])
  }

getd :: Eval Datum
getd = Eval $ \ds c -> case ds of
  (d : ds') -> Right (Ok d, c, ds')
  [] -> Left $ SyntaxError "Unexpected end of input"

end :: Eval ()
end = Eval $ \ds c -> case ds of
  [] -> Right (Ok (), c, [])
  _ -> Left $ SyntaxError "Unexpected token"

reval :: Eval a -> [Datum] -> Eval a
reval a da = Eval $ \ds c -> case eval a da c of
  Left e -> Left e
  Right (r, c', _) -> Right (r, c', ds)

fetch :: String -> Eval Value
fetch s = Eval $ \ds c ->
  case Map.lookup s c of
    Just v -> Right (Ok v, c, ds)
    Nothing -> Left $ SyntaxError $ "Undefined variable: " ++ s

define :: String -> Value -> Eval ()
define s v = Eval $ \ds c -> Right (Ok (), Map.insert s v c, ds)

store :: String -> Value -> Eval ()
store s v = Eval $ \ds c ->
  if Map.member s c
    then Right (Ok (), Map.insert s v c, ds)
    else Right (Exception $ Symbol "&undefined", c, ds)

syntaxError :: String -> Eval a
syntaxError s = Eval $ \_ _ -> Left $ SyntaxError s

raise :: Datum -> Eval a
raise e = Eval $ \ds c -> Right (Exception e, c, ds)

instance Functor Eval where
  fmap f a = Eval $ \ds c -> case eval a ds c of
    Left e -> Left e
    Right (Ok v, c', ds') -> Right (Ok $ f v, c', ds')
    Right (Exception d, c', ds') -> Right (Exception d, c', ds')

instance Applicative Eval where
  pure a = Eval $ \ds c -> Right (Ok a, c, ds)
  f <*> a = Eval $ \ds c -> case eval f ds c of
    Left e -> Left e
    Right (Exception e, c', ds') -> Right (Exception e, c', ds')
    Right (Ok f', c', ds') -> case eval a ds' c' of
      Left e -> Left e
      Right (Exception e, c'', ds'') -> Right (Exception e, c'', ds'')
      Right (Ok a', c'', ds'') -> Right (Ok $ f' a', c'', ds'')

instance Monad Eval where
  a >>= f = Eval $ \ds c -> case eval a ds c of
    Left e -> Left e
    Right (Exception e, c', ds') -> Right (Exception e, c', ds')
    Right (Ok a', c', ds') -> eval (f a') ds' c'

instance Alternative Eval where
  empty = syntaxError "Syntax error"
  a <|> b = Eval $ \ds c -> case eval a ds c of
    Left _ -> eval b ds c
    Right r -> Right r

unexpected :: Eval a
unexpected = do
  d <- getd
  syntaxError $ "Unexpected " ++ show d

expected :: String -> Eval a
expected s = do
  d <- getd
  syntaxError $ "Expected " ++ s ++ ", got " ++ show d

match :: (Datum -> Bool) -> Eval Datum
match f = do
  d <- getd
  if f d
    then return d
    else unexpected

nil :: Eval ()
nil = void $ match (== Empty)

boolean :: Eval Bool
boolean = do
  d <- getd
  case d of
    Boolean b -> return b
    _ -> expected "boolean"

number :: Eval Double
number = do
  d <- getd
  case d of
    Number n -> return n
    _ -> expected "number"

character :: Eval Char
character = do
  d <- getd
  case d of
    Character c -> return c
    _ -> expected "character"

string :: Eval String
string = do
  d <- getd
  case d of
    String s -> return s
    _ -> expected "string"

constant :: Eval Datum
constant =
  Boolean <$> boolean
    <|> Number <$> number
    <|> Character <$> character
    <|> String <$> string
    <|> expected "constant"

variable :: Eval String
variable = do
  d <- getd
  case d of
    Symbol s -> return s
    _ -> expected "identifier"

symbol :: String -> Eval String
symbol s = do
  v <- variable
  if v == s
    then return v
    else expected $ "`" ++ s ++ "`"

pair :: Eval (Datum, Datum)
pair = do
  d <- getd
  case d of
    Cons a b -> return (a, b)
    _ -> expected "pair"

improperList :: Eval ([Datum], Datum)
improperList = do
  (a, b) <- pair
  case b of
    Cons c d -> do
      (a', b') <- reval improperList [Cons c d]
      return (a : a', b')
    l -> return ([a], l)

improperWrap :: Eval (Eval a) -> Eval a
improperWrap a = do
  (h, t) <- improperList
  f <- reval (a <* end) h
  reval (f <* end) [t]

properList :: Eval [Datum]
properList =
  nil $> []
    <|> do
      (car, cdr) <- pair
      tail' <- reval properList [cdr]
      return (car : tail')

properWrap :: Eval a -> Eval a
properWrap a = properList >>= reval (a <* end)

program :: Eval ()
program = void $ many form

form :: Eval (Maybe Value)
form = definition $> Nothing <|> Just <$> expression

definition :: Eval ()
definition =
  variableDefinition
    <|> properWrap (symbol "begin" >> many definition $> ())

variableDefinition :: Eval ()
variableDefinition = properWrap $ do
  _ <- symbol "define"
  v <- variable
  e <- expression
  define v e

expression :: Eval Value
expression =
  Datum <$> constant
    <|> (variable >>= fetch)
    <|> properWrap (quote <|> lambda <|> branch <|> assignment <|> application)

quote :: Eval Value
quote = Datum <$> (symbol "quote" >> getd)

lambda :: Eval Value
lambda = do
  _ <- symbol "lambda"
  params <- formals
  bodies <- some getd
  return $ Procedure params bodies

formals :: Eval Formals
formals =
  Variadic [] <$> variable
    <|> Exact <$> properWrap (many variable)
    <|> improperWrap
      ( do
          vars <- some variable
          return $ Variadic vars <$> variable
      )

assignment :: Eval Value
assignment =
  symbol "set!" >> do
    v <- variable
    e <- expression
    store v e
    return e

application :: Eval Value
application = do
  e <- expression
  proc' <- case e of
    Procedure for das -> return $ apply for das
    _ -> raise $ Symbol "&error (not a procedure)"
  args <- many expression
  proc' args

branch :: Eval Value
branch =
  symbol "if" >> do
    cond <- expression
    if cond /= Datum (Boolean False)
      then expression <* getd
      else getd *> expression

apply :: Formals -> [Datum] -> [Value] -> Eval Value
apply _ [] _ = syntaxError "Procedure has no body"
apply (Exact []) [b] [] = reval (expression <* end) [b]
apply _ [_] _ = syntaxError "Not implemented"
apply for (b : bs) args = apply for [b] args >> apply for bs args
