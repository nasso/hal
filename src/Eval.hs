module Eval
  ( module Eval,
  )
where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Datum
import My.Control.Monad.Trans.ErrorT
import My.Control.Monad.Trans.ReaderT
import My.Control.Monad.Trans.StateT
import Program

-- | The environment maps symbols to values in the heap.
newtype Env = Env (Map String Int) deriving (Eq, Show)

-- | An empty environment.
emptyEnv :: Env
emptyEnv = Env Map.empty

-- | Represents any value in the heap.
data Value
  = Datum Datum
  | Closure Env ([Value] -> Eval Value)

instance Show Value where
  show (Datum d) = show d
  show (Closure _ _) = "#<procedure>"

-- | Storage for all allocated values.
newtype Heap = Heap (IntMap Value)

-- | An empty heap.
emptyHeap :: Heap
emptyHeap = Heap IntMap.empty

type Eval a = StateT Heap (ReaderT Env (ErrorT String IO)) a

runEval :: Eval a -> IO (Either String a)
runEval e = do
  v <- runErrorT $ runReaderT (runStateT e emptyHeap) emptyEnv
  return $ fst <$> v

-- | Chain evaluations and return the output of the last evaluation.
foldToLast :: NonEmpty (Eval a) -> Eval a
foldToLast = foldr1 (>>)

-- | Allocate a new value in the heap and return its address.
alloc :: Value -> Eval Int
alloc = error "alloc not implemented"

-- | Bind a symbol to a value in the heap.
bind :: String -> Int -> Eval ()
bind = error "bind not implemented"

-- | Define a new variable in the current environment.
define :: String -> Value -> Eval ()
define n v = alloc v >>= bind n

-- | Evaluates a program.
evalProgram :: Program -> Eval (Maybe Value)
evalProgram (Program []) = return Nothing
evalProgram (Program (f : fs)) =
  -- Evaluate each form, return the last value.
  foldToLast $ evalForm <$> f :| fs

-- | Evaluates a form (either a definition or an expression).
evalForm :: Form -> Eval (Maybe Value)
evalForm (Def d) = Nothing <$ evalDef d
evalForm (Expr e) = Just <$> evalExpr e

-- | Evaluates a definition.
evalDef :: Definition -> Eval ()
evalDef (VarDef v e) = do
  v' <- evalExpr e -- Evaluate the expression.
  p <- alloc v' -- Store the value in the heap.
  bind v p -- Bind the pointer to the variable.
evalDef (Begin defs) = mapM_ evalDef defs -- Evaluate each definition.

-- | Evaluates an expression.
evalExpr :: Expression -> Eval Value
evalExpr = error "evalExpr not implemented"
