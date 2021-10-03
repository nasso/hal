module Eval
  ( module Eval,
  )
where

import Control.Monad
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
alloc v = do
  h <- get
  let (a, h') = Heap.alloc h v
  put h'
  return a

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

-- | Evaluates a program.
evalProgram :: Program -> Eval a -> Eval a
evalProgram (Program []) e = e
evalProgram (Program [f]) e = evalForm f (return id) >> e
evalProgram (Program (f : fs)) e =
  evalForm f $ const <$> evalProgram (Program fs) e

-- | Evaluate a form and run @e@ in the potentially modified environment.
evalForm :: Form -> Eval (Maybe Value -> a) -> Eval a
evalForm (Expr expr) e = e <*> (Just <$> evalExpr expr)
evalForm (Def def) e = evalDef def $ e <*> return Nothing

-- | Evaluates a definition.
evalDef :: Definition -> Eval a -> Eval a
evalDef b ev = do
  bindings <- unroll b >>= mapM (prealloc . fst)
  bindAll bindings ev
  where
    prealloc v = (,) v <$> alloc (Datum $ Datum.Bool False)
    unroll (VarDef n e) = return [(n, e)]
    unroll (Begin ds) = join <$> mapM unroll ds

-- | Evaluates an expression.
evalExpr :: Expression -> Eval Value
evalExpr = error "evalExpr not implemented"
