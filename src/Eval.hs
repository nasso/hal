module Eval
  ( module Eval,
  )
where

import Control.Monad
import Data.Functor
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Datum
import Heap (Heap)
import qualified Heap
import My.Control.Monad.Trans.ErrorT
import My.Control.Monad.Trans.ReaderT
import My.Control.Monad.Trans.StateT
import Program

-- | The environment maps symbols to values in the heap.
type Env = Map Var Int

-- | An empty environment.
emptyEnv :: Env
emptyEnv = Map.empty

-- | Represents any value in the heap.
data Value
  = Datum Datum
  | Closure Env ([Value] -> Eval Value)

instance Show Value where
  show (Datum d) = show d
  show (Closure _ _) = "#<procedure>"

type Eval a = StateT (Heap Value) (ReaderT Env (ErrorT String IO)) a

runEval :: Eval a -> IO (Either String a)
runEval e = do
  v <- runErrorT $ runReaderT (runStateT e Heap.empty) emptyEnv
  return $ fst <$> v

-- | Chain evaluations and return the output of the last evaluation.
foldToLast :: NonEmpty (Eval a) -> Eval a
foldToLast = foldr1 (>>)

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

-- | Set the values of variables in the environment.
setAll :: [(Var, Value)] -> Eval ()
setAll [] = return ()
setAll ((n, v) : xs) = set n v >> setAll xs

-- | Evaluates a program.
evalProgram :: Program -> Eval a -> Eval a
evalProgram (Program []) e = e
evalProgram (Program [f]) e = evalForm f (const e)
evalProgram (Program (f : fs)) e =
  evalForm f $ const $ evalProgram (Program fs) e

-- | Evaluate a form and run @e@ in the potentially modified environment.
evalForm :: Form -> (Maybe Value -> Eval a) -> Eval a
evalForm (Expr expr) e = evalExpr expr >>= e . Just
evalForm (Def def) e = evalDef def $ e Nothing

-- | Evaluates a definition.
evalDef :: Definition -> Eval a -> Eval a
evalDef b ev = do
  pairs <- unroll b
  bindings <- mapM (prealloc . fst) pairs
  bindAll bindings (setAllExprs pairs >> ev)
  where
    prealloc v = (,) v <$> alloc (Datum $ Datum.Bool False)
    unroll (VarDef n e) = return [(n, e)]
    unroll (Begin ds) = join <$> mapM unroll ds
    setAllExprs [] = return ()
    setAllExprs ((n, e) : xs) = evalExpr e >>= set n >> setAllExprs xs

-- | Evaluates an expression.
evalExpr :: Expression -> Eval Value
evalExpr (Lit (Program.Bool b)) = return $ Datum $ Datum.Bool b
evalExpr (Lit (Program.Number i)) = return $ Datum $ Datum.Number i
evalExpr (Lit (Program.String s)) = return $ Datum $ Datum.String s
evalExpr (Lit (Program.Char c)) = return $ Datum $ Datum.Char c
evalExpr (Sym s) = deref s
evalExpr (Quote d) = return $ Datum d
evalExpr (If cond then' else') = do
  b <- evalExpr cond
  case b of
    Datum (Datum.Bool False) -> evalExpr else'
    _ -> evalExpr then'
evalExpr (Set var expr) = do
  val <- evalExpr expr
  set var val $> val
evalExpr (Lambda formals body) = do
  env <- ask -- capture the environment
  return $ -- create a closure
    Closure env $ \args -> do
      addrs <- allocAll args -- box all arguments
      bindFormals formals addrs $ evalBody body -- bind args and eval the body
evalExpr (Application funExpr argExprs) = do
  val <- evalExpr funExpr
  args <- mapM evalExpr argExprs
  case val of
    Closure env fn -> local (const env) $ fn args
    _ -> throwError "not a procedure"

-- | Bind the formals of a lambda expression some addresses.
bindFormals :: Formals -> [Int] -> Eval a -> Eval a
bindFormals (Exact []) [] e = e
bindFormals (Exact []) _ _ = throwError "too many arguments"
bindFormals (Exact _) [] _ = throwError "not enough arguments"
bindFormals (Exact (p : ps)) (a : as) e = bind p a $ bindFormals (Exact ps) as e
bindFormals (Variadic _ _) _ _ = error "variadics not supported"

-- | Evaluates a body, that is, a sequence of expresion, and returns the value
-- of the last expression.
evalBody :: NonEmpty Expression -> Eval Value
evalBody = foldToLast . fmap evalExpr
