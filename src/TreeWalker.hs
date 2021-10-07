module TreeWalker
  ( Eval,
    Value (..),
    Continuation,
    alloc,
    allocAll,
    bind,
    bindAll,
    define,
    defineAll,
    emptyEnv,
    evalForm,
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
import Control.Monad.MyTrans.ExceptT
import Control.Monad.MyTrans.ReaderT
import Control.Monad.MyTrans.StateT
import Data.Functor
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Datum (Datum)
import qualified Datum
import Heap (Heap)
import qualified Heap
import Number
import Program
  ( Definition (..),
    Expression (..),
    Form (..),
    Formals (..),
    Program (..),
    Var,
  )

-- | The environment maps symbols to values in the heap.
type Env = Map Var Int

-- | An empty environment.
emptyEnv :: Env
emptyEnv = Map.empty

type Continuation = [Value] -> Eval [Value]

fromCont1 :: (Value -> Eval [Value]) -> Continuation
fromCont1 f [v] = f v
fromCont1 _ l =
  throwError $
    "returned "
      ++ show (length l)
      ++ " values to a single value return context"

toCont1 :: Continuation -> Value -> Eval [Value]
toCont1 cont v = cont [v]

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
  | Procedure ([Int] -> Continuation -> Eval [Value])

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

type Eval a = StateT (Heap Value) (ReaderT Env (ExceptT String IO)) a

runEval :: Eval a -> IO (Either String a)
runEval e = do
  v <- runErrorT $ runReaderT (runStateT e Heap.empty) emptyEnv
  return $ fst <$> v

-- | Allocate a new value in the heap and return its address.
alloc :: Value -> Eval Int
alloc v = state (Heap.alloc v)

-- | Allocate many values in the heap and return their addresses.
allocAll :: [Value] -> Eval [Int]
allocAll [] = return []
allocAll (v : vs) = do
  a <- alloc v
  as <- allocAll vs
  return (a : as)

-- | Run an evaluator with a symbol bound to the address of a heap value.
bind :: Var -> Int -> Eval a -> Eval a
bind s a = local (Map.insert s a)

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
  v <- gets (Heap.fetch a)
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
store v a = modify (Heap.store a v)

-- | Get the address of a variable in the environment.
ref :: Var -> Eval Int
ref s = do
  a <- asks (Map.lookup s)
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
evalProgram :: Program -> Eval () -> Eval ()
evalProgram (Program []) e = e
evalProgram (Program (f : fs)) e =
  evalForm f $ const $ evalProgram (Program fs) e

-- | Evaluate a form and run @e@ in the potentially modified environment.
evalForm :: Form -> ([Value] -> Eval ()) -> Eval ()
evalForm (Expr expr) e = void $ evalExpr expr $ \v -> e v $> []
evalForm (Def def) e = evalDef def $ e []

-- | Evaluates a definition.
evalDef :: Definition -> Eval () -> Eval ()
evalDef b ev = do
  pairs <- unroll b
  bindings <- mapM (prealloc . fst) pairs
  bindAll bindings $ setps pairs
  where
    prealloc v = (,) v <$> alloc Void
    unroll (VarDef n e) = return [(n, e)]
    unroll (Begin ds) = join <$> mapM unroll ds
    setps [] = ev
    setps ((n, e) : xs) =
      (evalExpr e =<< close (fromCont1 $ \v -> set n v >> setps xs $> [])) $> ()

-- | Evaluates an expression.
evalExpr :: Expression -> Continuation -> Eval [Value]
evalExpr (Lit (Datum.Sym s)) cont = deref s >>= toCont1 cont
evalExpr (Lit c) cont = toCont1 cont $ valueFromDatum $ Datum.Lexeme c
evalExpr (Quote d) cont = toCont1 cont $ valueFromDatum d
evalExpr (Lambda formals body) cont = do
  env <- ask -- capture the environment
  cont
    [ Procedure $ -- create a procedure
        \args cont' ->
          local (const env) $ -- restore the captured environment
            bindFormals formals args $ -- bind arg values to formal params
              evalBody body cont' -- evaluate the body
    ]
evalExpr (Set var expr) cont = do
  cont' <- close cont -- lock the continuation to the current environment
  let assign val = set var val >> cont' []
   in evalExpr expr =<< close (fromCont1 assign)
evalExpr (If cond then' else') cont = do
  cont' <- close cont
  let branch (Bool False) = evalExpr else' cont'
      branch _ = evalExpr then' cont'
   in evalExpr cond =<< close (fromCont1 branch)
evalExpr (Application funExpr argExprs) cont = do
  cont' <- close cont
  let applyProc (Procedure proc') = evalArgs argExprs =<< close (run proc')
      applyProc _ = throwError "not a procedure"
      evalArgs [] c = c []
      evalArgs (e : es) c =
        evalExpr e =<< close (\v -> evalArgs es =<< close (c . (++) v))
      run proc' vs = allocAll vs >>= flip proc' cont'
   in evalExpr funExpr =<< close (fromCont1 applyProc)

-- | Bind the formals of a lambda expression some addresses.
bindFormals :: Formals -> [Int] -> Eval a -> Eval a
bindFormals (Strict []) [] e = e
bindFormals (Strict []) _ _ = throwError "too many arguments"
bindFormals (Strict _) [] _ = throwError "not enough arguments"
bindFormals (Strict (p : ps)) (a : as) e =
  bind p a $ bindFormals (Strict ps) as e
bindFormals (Variadic [] ps) as e = do
  avs <- fetchAll as
  define ps (foldr Pair Empty avs) e
bindFormals (Variadic (p : ps) ps') (a : as) e = do
  av <- fetch a
  define p av $ bindFormals (Variadic ps ps') as e
bindFormals (Variadic _ _) [] _ = throwError "not enough arguments"

-- | Evaluates a body, that is, a sequence of expressions, and returns the value
-- of the last expression.
evalBody :: NonEmpty Expression -> Continuation -> Eval [Value]
evalBody (e :| []) c = evalExpr e c
evalBody (e :| e' : es) c = evalExpr e $ const $ evalBody (e' :| es) c

close :: Continuation -> Eval Continuation
close c = do
  env <- ask
  return $ local (const env) . c
