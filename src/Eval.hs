module Eval
  ( newContext,
    fromScope,
    program,
    form,
    reval,
    eval,
    evalError,
    raise,
    define,
    fetch,
    assign,
    EvalError (..),
    EvalResult (..),
    Eval,
    Context,
    Value (..),
  )
where

import Control.Applicative
import Data.Functor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Datum (Datum (..))

data Formals = Exact [String] | Variadic [String] String deriving (Show, Eq)

data Value = Datum Datum | Procedure ([Value] -> Eval Value)

instance Show Value where
  show (Datum d) = show d
  show (Procedure _) = "#<procedure>"

eqv :: Value -> Value -> Bool
eqv (Datum Empty) (Datum Empty) = True
eqv (Datum (Char c)) (Datum (Char c')) = c == c'
eqv (Datum (Number n)) (Datum (Number n')) = n == n'
eqv (Datum (Bool b)) (Datum (Bool b')) = b == b'
eqv (Datum (String s)) (Datum (String s')) = s == s'
eqv (Datum (Symbol s)) (Datum (Symbol s')) = s == s'
eqv (Datum (Cons a b)) (Datum (Cons a' b')) =
  Datum a `eqv` Datum a' && Datum b `eqv` Datum b'
eqv _ _ = False

data Context = Context
  { scope :: Map String Value,
    parent :: Maybe Context
  }
  deriving (Show)

newContext :: Context
newContext = fromScope Map.empty

fromScope :: Map String Value -> Context
fromScope m = Context {scope = m, parent = Nothing}

data EvalError
  = SyntaxError
  | Extra
  | Eof
  | Expected String Value
  | Undefined String
  | CompoundError EvalError EvalError

instance Show EvalError where
  show SyntaxError = "syntax error"
  show Extra = "extra input"
  show Eof = "unexpected end of input"
  show (Expected v got) = "expected " ++ v ++ ", got " ++ show got
  show (Undefined name) = "undefined reference(s) to " ++ name
  show (CompoundError e1 e2) = show e1 ++ "; " ++ show e2

cross' :: EvalError -> EvalError -> EvalError
cross' SyntaxError e = e
cross' e SyntaxError = e
cross' Extra e = e
cross' e Extra = e
cross' Eof e = e
cross' e Eof = e
cross' (Expected s v) (Expected s' v')
  | v `eqv` v' =
    if s == s'
      then Expected s v
      else Expected (s ++ "/" ++ s') v
cross' (Undefined s) (Undefined s') | s == s' = Undefined s
cross' (Undefined s) (Undefined s') = Undefined $ s ++ ", " ++ s'
cross' (Expected _ _) (Undefined s') = Undefined s'
cross' (Undefined s) (Expected _ _) = Undefined s
cross' e1 e2 = CompoundError e1 e2

cross :: EvalError -> EvalError -> EvalError
cross (CompoundError e1 e2) e = cross' (cross e1 e) (cross e2 e)
cross e (CompoundError e1 e2) = cross' (cross e1 e) (cross e2 e)
cross e1 e2 = cross' e1 e2

data EvalResult a = Ok a | Exception Datum deriving (Show, Eq)

newtype Eval a = Eval
  { eval ::
      [Datum] ->
      Context ->
      Either EvalError (EvalResult a, Context, [Datum])
  }

getContext :: Eval Context
getContext = Eval $ \ds ctx -> Right (Ok ctx, ctx, ds)

setContext :: Context -> Eval ()
setContext ctx = Eval $ \ds _ -> Right (Ok (), ctx, ds)

getd :: Eval Datum
getd = Eval $ \ds c -> case ds of
  (d : ds') -> Right (Ok d, c, ds')
  [] -> Left Eof

end :: Eval ()
end = Eval $ \ds c -> case ds of
  [] -> Right (Ok (), c, [])
  _ -> Left Extra

reval :: Eval a -> [Datum] -> Eval a
reval a da = Eval $ \ds c -> case eval a da c of
  Left e -> Left e
  Right (r, c', _) -> Right (r, c', ds)

evalError :: EvalError -> Eval a
evalError e = Eval $ \_ _ -> Left e

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
  empty = evalError SyntaxError
  a <|> b = Eval $ \ds c -> case eval a ds c of
    Right r -> Right r
    Left e -> case eval b ds c of
      Right r -> Right r
      Left e' -> Left $ cross e e'

beginScope :: Eval ()
beginScope = do
  ctx <- getContext
  setContext $ Context {scope = Map.empty, parent = Just ctx}

endScope :: Eval ()
endScope = do
  ctx <- getContext
  case parent ctx of
    Nothing -> evalError SyntaxError
    Just ctx' -> setContext ctx'

makeScope :: Eval Value -> Eval Value
makeScope e = beginScope *> e <* endScope

using :: Eval a -> Context -> Eval a
using a ctx = do
  ctx' <- getContext
  setContext ctx *> a <* setContext ctx'

fetch :: String -> Eval Value
fetch varname = do
  ctx <- getContext
  case (Map.lookup varname (scope ctx), parent ctx) of
    (Just v, _) -> return v
    (Nothing, Just ctx') -> fetch varname `using` ctx'
    _ -> evalError $ Undefined varname

-- | Bind a value to a symbol
define :: String -> Value -> Eval ()
define s v = do
  ctx <- getContext
  setContext $ ctx {scope = Map.insert s v (scope ctx)}

-- | Set the value of an existing variable
assign :: String -> Value -> Eval ()
assign var val = do
  ctx <- getContext
  if Map.member var (scope ctx)
    then setContext $ ctx {scope = Map.insert var val (scope ctx)}
    else case parent ctx of
      Just pctx -> do
        pctx' <- (assign var val >> getContext) `using` pctx
        setContext $ ctx {parent = Just pctx'}
      Nothing -> evalError $ Undefined var

expected :: String -> Eval a
expected s = do
  d <- getd
  evalError $ Expected s $ Datum d

nil :: Eval ()
nil = do
  d <- getd
  case d of
    Empty -> return ()
    _ -> evalError $ Expected "'()" $ Datum d

boolean :: Eval Bool
boolean = do
  d <- getd
  case d of
    Bool b -> return b
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
    Char c -> return c
    _ -> expected "character"

string :: Eval String
string = do
  d <- getd
  case d of
    String s -> return s
    _ -> expected "string"

constant :: Eval Datum
constant =
  Bool <$> boolean
    <|> Number <$> number
    <|> Char <$> character
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

program :: Eval (Maybe Value)
program = end $> Nothing <|> form <* end <|> form *> program

form :: Eval (Maybe Value)
form = definition $> Nothing <|> Just <$> expression

definition :: Eval ()
definition =
  variableDefinition <|> properWrap (symbol "begin" >> many definition $> ())

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
  body <- some getd
  return $ Procedure $ lambdaRunner params body

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
    assign v e
    return e

application :: Eval Value
application = do
  e <- expression
  case e of
    Procedure p -> many expression >>= p
    d -> raise $ Symbol $ "&error (not a procedure): " ++ show d

branch :: Eval Value
branch =
  symbol "if" >> do
    cond <- expression
    case cond of
      Datum (Bool False) -> getd *> expression
      _ -> expression <* getd

lambdaRunner :: Formals -> [Datum] -> [Value] -> Eval Value
lambdaRunner _ [] _ = evalError SyntaxError
lambdaRunner f b args =
  makeScope
    ( defineFormals f args
        *> reval (body <* end) b
    )
  where
    body = do
      e <- expression
      body <|> return e

defineFormals :: Formals -> [Value] -> Eval ()
defineFormals (Exact []) [] = return ()
defineFormals (Exact (s : s')) (a : a') =
  define s a >> defineFormals (Exact s') a'
defineFormals (Exact _) [] = raise $ Symbol "&error (too few arguments)"
defineFormals (Exact []) _ = raise $ Symbol "&error (too many arguments)"
defineFormals (Variadic [] _var) _args = raise $ Symbol "&unsupported"
defineFormals (Variadic (s : s') var) (a : a') =
  define s a >> defineFormals (Variadic s' var) a'
defineFormals (Variadic _ _) [] = raise $ Symbol "&error (too few arguments)"
