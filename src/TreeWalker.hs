module TreeWalker
  ( Eval,
    Value (..),
    getExpandCtx,
    withExpandCtx,
    valueFromDatum,
    datumFromValue,
    alloc,
    allocAll,
    bind,
    bindAll,
    define,
    defineAll,
    emptyEnv,
    evalProgram,
    fetch,
    fetchAll,
    store,
    runEval,
    ref,
    deref,
    set,
  )
where

import Control.Monad
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Datum (Datum)
import qualified Datum
import Expand (ExpandCtx, defaultExpandCtx, setVars)
import Heap (Heap)
import qualified Heap
import Number
import Syntax (Expression, Form, Formals, Program, Var)
import qualified Syntax

-- | The environment maps symbols to values in the heap.
data Env = Env
  { vars :: Map Var Int,
    expandCtx :: ExpandCtx
  }

-- | An empty environment.
emptyEnv :: Env
emptyEnv = Env Map.empty defaultExpandCtx

-- | Get an up-to-date expansion context for the current environment.
getExpandCtx :: Eval ExpandCtx
getExpandCtx = setVars <$> asks (Map.keys . vars) <*> asks expandCtx

withExpandCtx :: ExpandCtx -> Eval a -> Eval a
withExpandCtx ctx = local (\env -> env {expandCtx = ctx})

-- | Represents any value in the heap.
data Value
  = Void
  | Bool Bool
  | Number Number
  | Char Char
  | String String
  | Symbol String
  | Pair Value Value
  | Empty
  | Procedure ([Int] -> Eval [Value])

instance Show Value where
  show Void = "#<void>"
  show (Pair (Symbol "quote") (Pair v Empty)) = "'" ++ show v
  show (Pair (Symbol "quasiquote") (Pair v Empty)) = "`" ++ show v
  show (Pair (Symbol "unquote") (Pair v Empty)) = "," ++ show v
  show (Pair (Symbol "unquote-splicing") (Pair v Empty)) = ",@" ++ show v
  show (Pair (Symbol "syntax") (Pair v Empty)) = "#'" ++ show v
  show (Pair (Symbol "quasisyntax") (Pair v Empty)) = "#`" ++ show v
  show (Pair (Symbol "unsyntax") (Pair v Empty)) = "#," ++ show v
  show (Pair (Symbol "unsyntax-splicing") (Pair v Empty)) = "#,@" ++ show v
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Number n) = show n
  show (Char '\x0007') = "#\\alarm"
  show (Char '\x0008') = "#\\backspace"
  show (Char '\x007f') = "#\\delete"
  show (Char '\x001b') = "#\\esc"
  show (Char '\x000a') = "#\\newline"
  show (Char '\x000c') = "#\\page"
  show (Char '\x000d') = "#\\return"
  show (Char ' ') = "#\\space"
  show (Char '\t') = "#\\tab"
  show (Char '\v') = "#\\vtab"
  show (Char c) = "#\\" ++ [c]
  show (String s) = show s
  show (Symbol s) = s
  show (Pair car cdr) = "(" ++ show car ++ expand cdr ++ ")"
    where
      expand :: Value -> String
      expand Empty = ""
      expand (Pair car' cdr') = " " ++ show car' ++ expand cdr'
      expand d = " . " ++ show d
  show Empty = "()"
  show (Procedure _) = "#<procedure>"

valueFromDatum :: Datum -> Value
valueFromDatum (Datum.Lexeme (Datum.Bool b)) = Bool b
valueFromDatum (Datum.Lexeme (Datum.Number n)) = Number n
valueFromDatum (Datum.Lexeme (Datum.Char c)) = Char c
valueFromDatum (Datum.Lexeme (Datum.String s)) = String s
valueFromDatum (Datum.Lexeme (Datum.Sym s)) = Symbol s
valueFromDatum (Datum.List ds) = foldr Pair Empty $ valueFromDatum <$> ds
valueFromDatum (Datum.ImproperList ds tl) =
  foldr Pair (valueFromDatum (Datum.Lexeme tl)) $ valueFromDatum <$> ds

datumFromValue :: Value -> Maybe Datum
datumFromValue Void = Nothing
datumFromValue Empty = Just $ Datum.List []
datumFromValue (Bool b) = Just $ Datum.Lexeme $ Datum.Bool b
datumFromValue (Number n) = Just $ Datum.Lexeme $ Datum.Number n
datumFromValue (Char c) = Just $ Datum.Lexeme $ Datum.Char c
datumFromValue (String s) = Just $ Datum.Lexeme $ Datum.String s
datumFromValue (Symbol s) = Just $ Datum.Lexeme $ Datum.Sym s
datumFromValue (Pair car Empty) = do
  car' <- datumFromValue car
  return $ Datum.List [car']
datumFromValue (Pair car cdr) = do
  car' <- datumFromValue car
  cdr' <- datumFromValue cdr
  return $ case cdr' of
    Datum.List das -> Datum.List (car' : das)
    Datum.Lexeme con -> Datum.ImproperList (car' :| []) con
    Datum.ImproperList (e :| es) con ->
      Datum.ImproperList (car' :| e : es) con
datumFromValue (Procedure _) = Nothing

newtype EvalState = EvalState
  { heap :: Heap Value
  }
  deriving (Show)

emptyState :: EvalState
emptyState = EvalState Heap.empty

heapState :: (Heap Value -> (a, Heap Value)) -> Eval a
heapState f = state go
  where
    go s = (v, s {heap = h}) where (v, h) = f (heap s)

heapGets :: (Heap Value -> a) -> Eval a
heapGets f = gets (f . heap)

heapModify :: (Heap Value -> Heap Value) -> Eval ()
heapModify f = modify $ \s -> s {heap = f (heap s)}

type Eval a = ExceptT String (ReaderT Env (ContT () (StateT EvalState IO))) a

runEval :: Eval a -> (Either String a -> IO ()) -> IO ()
runEval e k =
  let r = runExceptT e
      c = runReaderT r emptyEnv
      s = runContT c (liftIO . k)
   in fst <$> runStateT s emptyState

-- | Throw an error if an evaluation returns more than one value.
single :: Eval [Value] -> Eval Value
single e = do
  vs <- e
  case vs of
    [v] -> return v
    _ -> throwError "expected single value"

-- | Allocate a new value in the heap and return its address.
alloc :: Value -> Eval Int
alloc v = heapState (Heap.alloc v)

-- | Allocate many values in the heap and return their addresses.
allocAll :: [Value] -> Eval [Int]
allocAll [] = return []
allocAll (v : vs) = do
  a <- alloc v
  as <- allocAll vs
  return (a : as)

-- | Run an evaluator with a symbol bound to the address of a heap value.
bind :: Var -> Int -> Eval a -> Eval a
bind s a = local $ \e -> e {vars = Map.insert s a $ vars e}

-- | Bind a list of pairs of symbols and addresses to the environment.
bindAll :: [(Var, Int)] -> Eval a -> Eval a
bindAll [] = id
bindAll ((s, a) : xs) = bind s a . bindAll xs

-- | Bind a variable to a new value in the environment in which @e@ is run.
define :: Var -> Value -> Eval a -> Eval a
define n v e = do a <- alloc v; bind n a e

-- | Bind variables to new values in the environment in which @e@ is run.
defineAll :: [(Var, Value)] -> Eval a -> Eval a
defineAll [] = id
defineAll ((n, v) : xs) = define n v . defineAll xs

-- | Fetch a value from the heap.
fetch :: Int -> Eval Value
fetch a = do
  v <- heapGets (Heap.fetch a)
  case v of
    Just v' -> return v'
    Nothing -> throwError "Segmentation fault."

-- | Fetch a list of values from the heap.
fetchAll :: [Int] -> Eval [Value]
fetchAll [] = return []
fetchAll (a : as) = do
  v <- fetch a
  vs <- fetchAll as
  return (v : vs)

-- | Assign a value to a heap address.
store :: Value -> Int -> Eval ()
store v a = heapModify (Heap.store a v)

-- | Get the address of a variable in the environment.
ref :: Var -> Eval Int
ref s = do
  a <- asks (Map.lookup s . vars)
  case a of
    Just a' -> return a'
    Nothing -> throwError $ "unbound variable: " ++ show s

-- | Dereference a variable in the environment.
deref :: Var -> Eval Value
deref n = ref n >>= fetch

-- | Set the value of a variable in the environment.
set :: Var -> Value -> Eval ()
set n v = ref n >>= store v

-- | Evaluates a program.
evalProgram :: Program -> ([Value] -> Eval ()) -> Eval ()
evalProgram (Syntax.Program []) e = e []
evalProgram (Syntax.Program [f]) e = evalForm f e
evalProgram (Syntax.Program (f : fs)) e =
  evalForm f $ const $ evalProgram (Syntax.Program fs) e

-- | Evaluate a form and run @e@ in the new environment.
evalForm :: Form -> ([Value] -> Eval ()) -> Eval ()
evalForm (Syntax.Expr expr) e = void $ evalExpr expr >>= e
evalForm (Syntax.Def def) e = evalDef def $ e []

-- | Evaluates a series of definition like @letrec*@, and run @ev@ in the new
-- environment.
evalDef :: [(Var, Expression)] -> Eval () -> Eval ()
evalDef defs ev = do
  bindings <- mapM (prealloc . fst) defs
  bindAll bindings $ setps defs
  where
    prealloc v = (,) v <$> alloc Void
    setps [] = ev
    setps ((n, e) : xs) = (single (evalExpr e) >>= set n) >> setps xs

-- | Evaluates an expression.
evalExpr :: Expression -> Eval [Value]
evalExpr (Syntax.Lit (Datum.Sym s)) = pure <$> deref s
evalExpr (Syntax.Lit c) = pure <$> pure $ valueFromDatum (Datum.Lexeme c)
evalExpr (Syntax.Quote d) = pure <$> pure $ valueFromDatum d
evalExpr (Syntax.Begin (e :| [])) = evalExpr e
evalExpr (Syntax.Begin (e :| e' : es)) =
  evalExpr e >> evalExpr (Syntax.Begin (e' :| es))
evalExpr (Syntax.Lambda formals body) =
  pure <$> pure . Procedure . run =<< ask
  where
    run env args =
      local (const env) $ -- restore the captured environment
        bindFormals formals args $ -- bind arg values to formal params
          evalBody body -- evaluate the body
evalExpr (Syntax.Set var expr) = (single (evalExpr expr) >>= set var) $> []
evalExpr (Syntax.If cond then' else') =
  single (evalExpr cond) >>= branch
  where
    branch (Bool False) = evalExpr else'
    branch _ = evalExpr then'
evalExpr (Syntax.Application funExpr argExprs) =
  single (evalExpr funExpr) >>= applyProc
  where
    applyProc (Procedure proc') = evalArgs argExprs >>= allocAll >>= proc'
    applyProc v = throwError $ "not a procedure: " ++ show v
    evalArgs [] = pure []
    evalArgs (e : es) = (:) <$> single (evalExpr e) <*> evalArgs es

-- | Bind the formals of a lambda expression some addresses.
bindFormals :: Formals -> [Int] -> Eval a -> Eval a
bindFormals (Syntax.Strict []) [] e = e
bindFormals (Syntax.Strict []) _ _ = throwError "too many arguments"
bindFormals (Syntax.Strict _) [] _ = throwError "not enough arguments"
bindFormals (Syntax.Strict (p : ps)) (a : as) e =
  bind p a $ bindFormals (Syntax.Strict ps) as e
bindFormals (Syntax.Variadic [] ps) as e = do
  avs <- fetchAll as
  define ps (foldr Pair Empty avs) e
bindFormals (Syntax.Variadic (p : ps) ps') (a : as) e = do
  av <- fetch a
  define p av $ bindFormals (Syntax.Variadic ps ps') as e
bindFormals (Syntax.Variadic _ _) [] _ = throwError "not enough arguments"

-- | Evaluates a body, that is, a sequence of expressions, and returns the value
-- of the last expression.
evalBody :: NonEmpty Expression -> Eval [Value]
evalBody (e :| []) = evalExpr e
evalBody (e :| e' : es) = evalExpr e >> evalBody (e' :| es)
