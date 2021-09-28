module Eval
  ( newContext,
    form,
    eval,
    throw,
    EvalError (..),
    Eval,
    Context,
    Value (..),
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Grammar.Datum (Datum (..))

data Formals = Exact [String] | Variadic [String] String deriving (Show, Eq)

data Value = Datum Datum | Procedure Formals [Datum] deriving (Eq)

instance Show Value where
  show (Datum d) = show d
  show (Procedure _ _) = "#<procedure>"

type Context = Map String Value

data EvalError
  = SyntaxError
  | Exception Datum
  deriving (Show, Eq)

newContext :: Context
newContext = Map.empty

newtype Eval a = Eval
  { eval :: Context -> Either EvalError (a, Context)
  }

instance Functor Eval where
  fmap f a = Eval $ \c -> case eval a c of
    Left e -> Left e
    Right (v, c') -> Right (f v, c')

instance Applicative Eval where
  pure a = Eval $ \c -> Right (a, c)
  f <*> a = Eval $ \c -> case eval f c of
    Left e -> Left e
    Right (f', c') -> case eval a c' of
      Left e -> Left e
      Right (a', c'') -> Right (f' a', c'')

instance Monad Eval where
  a >>= f = Eval $ \c -> case eval a c of
    Left e -> Left e
    Right (a', c') -> eval (f a') c'

instance Alternative Eval where
  empty = Eval $ \_ -> Left SyntaxError
  a <|> b = Eval $ \c -> case eval a c of
    Left _ -> eval b c
    Right a' -> Right a'

throw :: EvalError -> Eval a
throw e = Eval $ \_ -> Left e

raise :: Datum -> Eval a
raise = throw . Exception

define :: String -> Value -> Eval Value
define name value = Eval $ \c -> Right (value, Map.insert name value c)

fetch :: String -> Eval Value
fetch name = Eval $ \c -> case Map.lookup name c of
  Nothing -> Left . Exception . String $ "Unbound identifier: " ++ name
  Just v -> Right (v, c)

isDefined :: String -> Eval Bool
isDefined name = Eval $ \c -> Right (Map.member name c, c)

form :: Datum -> Eval Value
form v = definition v <|> expression v

definition :: Datum -> Eval Value
definition (Cons (Symbol "define") (Cons (Symbol s) (Cons e Empty))) =
  define s =<< form e
definition (Cons (Symbol "begin") v) = evalProper v
  where
    evalProper Empty = return $ Datum Empty
    evalProper (Cons e es) = definition e >> evalProper es
    evalProper _ = empty
definition _ = empty

expression :: Datum -> Eval Value
expression (Boolean b) = return $ Datum $ Boolean b
expression (Number n) = return $ Datum $ Number n
expression (Character c) = return $ Datum $ Character c
expression (String s) = return $ Datum $ String s
expression (Symbol s) = fetch s
expression (Cons (Symbol s) exprs) =
  do
    args <- collectProper exprs
    do
      p <- fetch s
      case p of
        Procedure params bodies -> call params bodies args
        _ -> empty
      <|> builtin s args
expression (Cons pexpr exprs) =
  do
    args <- collectProper exprs
    do
      p <- expression pexpr
      case p of
        Procedure params bodies -> call params bodies args
        _ -> empty
expression _ = empty

collectProper :: Datum -> Eval [Datum]
collectProper Empty = return []
collectProper (Cons x xs) = (x :) <$> collectProper xs
collectProper _ = empty

builtin :: String -> [Datum] -> Eval Value
builtin "quote" [v] = return $ Datum v
builtin "lambda" (Symbol args : body : bodies) =
  return $ Procedure (Variadic [] args) (body : bodies)
builtin "lambda" (formals : body : bodies) =
  case (paramNames, rest) of
    (Just ps, Empty) -> return $ Procedure (Exact ps) (body : bodies)
    (Just ps, Symbol r) -> return $ Procedure (Variadic ps r) (body : bodies)
    _ -> empty
  where
    (params, rest) = collect formals
    paramNames = mapM sym params
    sym (Symbol s) = return s
    sym _ = empty
builtin "if" [expr, then', else'] = do
  v <- form expr
  if v == Datum (Boolean False)
    then form else'
    else form then'
builtin "set!" [Symbol var, expr] = do
  exists <- isDefined var
  if exists
    then form expr >>= define var
    else empty
builtin _ _ = empty

collect :: Datum -> ([Datum], Datum)
collect (Cons v vs) = (v : rest, last') where (rest, last') = collect vs
collect v = ([], v)

call :: Formals -> [Datum] -> [Datum] -> Eval Value
call _ [] _ = raise $ String "Attempt to call a procedure with no body"
call (Exact []) [body] [] = form body
call (Exact []) (body : bodies) [] = form body >> call (Exact []) bodies []
call _ _ _ = raise $ String "Not implemented"
