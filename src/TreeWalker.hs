module TreeWalker
  ( Eval,
    Value (..),
    Continuation,
    valueFromDatum,
    datumFromValue,
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
import Control.Monad.MyTrans
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
  ( Expression (..),
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

type Eval a = ExceptT String (ReaderT Env (ContT () (StateT (Heap Value) IO))) a

runEval :: Eval a -> (Either String a -> IO ()) -> IO ()
runEval e k =
  let r = runExceptT e
      c = runReaderT r emptyEnv
      s = runContT c (liftIO . k)
   in fst <$> runStateT s Heap.empty

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
evalDef :: [(Var, Expression)] -> Eval () -> Eval ()
evalDef defs ev = do
  bindings <- mapM (prealloc . fst) defs
  bindAll bindings $ setps defs
  where
    prealloc v = (,) v <$> alloc Void
    setps [] = ev
    setps ((n, e) : xs) =
      evalExpr e (fromCont1 $ \v -> set n v >> setps xs $> []) $> ()

-- | Evaluates an expression.
evalExpr :: Expression -> Continuation -> Eval [Value]
evalExpr (Lit (Datum.Sym s)) cont = deref s >>= toCont1 cont
evalExpr (Lit c) cont = toCont1 cont $ valueFromDatum $ Datum.Lexeme c
evalExpr (Quote d) cont = toCont1 cont $ valueFromDatum d
evalExpr (Begin (e :| [])) cont = evalExpr e cont
evalExpr (Begin (e :| e' : es)) cont =
  evalExpr e $ const $ evalExpr (Begin (e' :| es)) cont
evalExpr (Lambda formals body) cont =
  let run env args cont' =
        close cont' -- capture the continuation's environment
          >>= local (const env) -- restore the captured environment
            . bindFormals formals args -- bind arg values to formal params
            . evalBody body -- evaluate the body
      close c = asks $ \e -> local (const e) . c
   in cont . (: []) . Procedure . run =<< ask
evalExpr (Set var expr) cont =
  let assign val = set var val >> cont []
   in evalExpr expr $ fromCont1 assign
evalExpr (If cond then' else') cont =
  let branch (Bool False) = evalExpr else' cont
      branch _ = evalExpr then' cont
   in evalExpr cond $ fromCont1 branch
evalExpr (Application funExpr argExprs) cont =
  let applyProc (Procedure proc') = evalArgs argExprs $ run proc'
      applyProc v = throwError $ "not a procedure: " ++ show v
      evalArgs [] c = c []
      evalArgs (e : es) c = evalExpr e $ \v -> evalArgs es $ c . (++) v
      run proc' vs = allocAll vs >>= flip proc' cont
   in evalExpr funExpr $ fromCont1 applyProc

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
