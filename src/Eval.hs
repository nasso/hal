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

store :: String -> Value -> Eval ()
store s v = Eval $ \ds c -> Right (Ok (), Map.insert s v c, ds)

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

form :: Eval Value
form = definition <|> expression

definition :: Eval Value
definition = variableDefinition

variableDefinition :: Eval Value
variableDefinition = properWrap $ do
  _ <- symbol "define"
  v <- variable
  e <- expression
  store v e
  return (Datum $ Symbol v)

expression :: Eval Value
expression =
  Datum <$> constant
    <|> (variable >>= fetch)
    <|> properWrap (quote <|> branch)

quote :: Eval Value
quote = Datum <$> (symbol "quote" >> getd)

branch :: Eval Value
branch =
  symbol "if" >> do
    cond <- expression
    if cond /= Datum (Boolean False)
      then expression <* getd
      else getd *> expression

-- define :: String -> Value -> Eval Value
-- define name value = Eval $ \c -> Right (value, Map.insert name value c)
--
-- fetch :: String -> Eval Value
-- fetch name = Eval $ \c -> case Map.lookup name c of
--   Nothing -> Left . Exception . String $ "Unbound identifier: " ++ name
--   Just v -> Right (v, c)
--
-- isDefined :: String -> Eval Bool
-- isDefined name = Eval $ \c -> Right (Map.member name c, c)
--
-- form :: Datum -> Eval Value
-- form v = definition v <|> expression v
--
-- definition :: Datum -> Eval Value
-- definition (Cons (Symbol "define") (Cons (Symbol s) (Cons e Empty))) =
--   define s =<< form e
-- definition (Cons (Symbol "begin") v) = evalProper v
--   where
--     evalProper Empty = return $ Datum Empty
--     evalProper (Cons e es) = definition e >> evalProper es
--     evalProper _ = empty
-- definition _ = empty
--
-- expression :: Datum -> Eval Value
-- expression (Boolean b) = return $ Datum $ Boolean b
-- expression (Number n) = return $ Datum $ Number n
-- expression (Character c) = return $ Datum $ Character c
-- expression (String s) = return $ Datum $ String s
-- expression (Symbol s) = fetch s
-- expression (Cons (Symbol s) exprs) =
--   do
--     args <- collectProper exprs
--     do
--       p <- fetch s
--       case p of
--         Procedure params bodies -> call params bodies args
--         _ -> empty
--       <|> builtin s args
-- expression (Cons pexpr exprs) =
--   do
--     args <- collectProper exprs
--     do
--       p <- expression pexpr
--       case p of
--         Procedure params bodies -> call params bodies args
--         _ -> empty
-- expression _ = empty
--
-- collectProper :: Datum -> Eval [Datum]
-- collectProper Empty = return []
-- collectProper (Cons x xs) = (x :) <$> collectProper xs
-- collectProper _ = empty
--
-- builtin :: String -> [Datum] -> Eval Value
-- builtin "quote" [v] = return $ Datum v
-- builtin "lambda" (Symbol args : body : bodies) =
--   return $ Procedure (Variadic [] args) (body : bodies)
-- builtin "lambda" (formals : body : bodies) =
--   case (paramNames, rest) of
--     (Just ps, Empty) -> return $ Procedure (Exact ps) (body : bodies)
--     (Just ps, Symbol r) -> return $ Procedure (Variadic ps r) (body : bodies)
--     _ -> empty
--   where
--     (params, rest) = collect formals
--     paramNames = mapM sym params
--     sym (Symbol s) = return s
--     sym _ = empty
-- builtin "if" [expr, then', else'] = do
--   v <- form expr
--   if v == Datum (Boolean False)
--     then form else'
--     else form then'
-- builtin "set!" [Symbol var, expr] = do
--   exists <- isDefined var
--   if exists
--     then form expr >>= define var
--     else empty
-- builtin _ _ = empty
--
-- collect :: Datum -> ([Datum], Datum)
-- collect (Cons v vs) = (v : rest, last') where (rest, last') = collect vs
-- collect v = ([], v)
--
-- call :: Formals -> [Datum] -> [Datum] -> Eval Value
-- call _ [] _ = raise $ String "Attempt to call a procedure with no body"
-- call (Exact []) [body] [] = form body
-- call (Exact []) (body : bodies) [] = form body >> call (Exact []) bodies []
-- call _ _ _ = raise $ String "Not implemented"
