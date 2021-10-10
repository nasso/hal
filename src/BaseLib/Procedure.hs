module BaseLib.Procedure
  ( baseProcedures,
  )
where

import Control.Monad
import Control.Monad.Cont.Class
import Control.Monad.Except.Class
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Data.Fixed
import Data.Functor
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Ratio (denominator, numerator)
import Expand
import Number
import Program (Var)
import TreeWalker

baseProcedures :: [(Var, Value)]
baseProcedures =
  builtinList ++ builtinPredicates ++ builtinArithmetics
    ++ builtinIO
    ++ builtinUtils
    ++ builtinDebug

cbvProc :: ([Value] -> Eval [Value]) -> Value
cbvProc f = Procedure $ fetchAll >=> f

cbrProc1 :: ([Int] -> Eval Value) -> Value
cbrProc1 f = Procedure $ fmap (: []) . f

cbvProc1 :: ([Value] -> Eval Value) -> Value
cbvProc1 f = cbvProc $ fmap (: []) . f

builtinList :: [(Var, Value)]
builtinList =
  [ ("cons", cbvProc1 builtinPair),
    ("car", cbvProc1 builtinCar),
    ("cdr", cbvProc1 builtinCdr)
  ]

builtinPair :: [Value] -> Eval Value
builtinPair [a, b] = return $ Pair a b
builtinPair _ = throwError "cons: wrong number of arguments"

builtinCar :: [Value] -> Eval Value
builtinCar [Pair car _] = return car
builtinCar _ = throwError "car: invalid arguments"

builtinCdr :: [Value] -> Eval Value
builtinCdr [Pair _ cdr] = return cdr
builtinCdr _ = throwError "cdr: invalid arguments"

builtinPredicates :: [(Var, Value)]
builtinPredicates =
  [ ("eq?", cbrProc1 (fmap Bool . builtinEq)),
    ("boolean?", cbvProc1 (fmap Bool . builtinIsBool)),
    ("number?", cbvProc1 (fmap Bool . builtinIsNumber)),
    ("symbol?", cbvProc1 (fmap Bool . builtinIsSymbol)),
    ("char?", cbvProc1 (fmap Bool . builtinIsChar)),
    ("string?", cbvProc1 (fmap Bool . builtinIsString)),
    ("pair?", cbvProc1 (fmap Bool . builtinIsPair)),
    ("null?", cbvProc1 (fmap Bool . builtinIsNull))
  ]

builtinIsBool :: [Value] -> Eval Bool
builtinIsBool [Bool _] = return True
builtinIsBool [_] = return False
builtinIsBool _ = throwError "boolean?: invalid arguments"

builtinIsNumber :: [Value] -> Eval Bool
builtinIsNumber [Number _] = return True
builtinIsNumber [_] = return False
builtinIsNumber _ = throwError "number?: invalid arguments"

builtinIsSymbol :: [Value] -> Eval Bool
builtinIsSymbol [Symbol _] = return True
builtinIsSymbol [_] = return False
builtinIsSymbol _ = throwError "symbol?: invalid arguments"

builtinIsChar :: [Value] -> Eval Bool
builtinIsChar [Char _] = return True
builtinIsChar [_] = return False
builtinIsChar _ = throwError "char?: invalid arguments"

builtinIsString :: [Value] -> Eval Bool
builtinIsString [String _] = return True
builtinIsString [_] = return False
builtinIsString _ = throwError "string?: invalid arguments"

builtinIsPair :: [Value] -> Eval Bool
builtinIsPair [Pair _ _] = return True
builtinIsPair [_] = return False
builtinIsPair _ = throwError "pair?: invalid arguments"

builtinIsNull :: [Value] -> Eval Bool
builtinIsNull [Empty] = return True
builtinIsNull [_] = return False
builtinIsNull _ = throwError "null?: invalid arguments"

builtinEq :: [Int] -> Eval Bool
builtinEq [a, b] | a == b = return True
builtinEq [a', b'] = cmp <$> fetch a' <*> fetch b'
  where
    cmp Void Void = True
    cmp Empty Empty = True
    cmp (Number a) (Number b) = a == b
    cmp (Bool a) (Bool b) = a == b
    cmp (Char a) (Char b) = a == b
    cmp (String a) (String b) = a == b
    cmp (Symbol a) (Symbol b) = a == b
    cmp _ _ = False
builtinEq _ = throwError "eq?: invalid arguments"

builtinArithmetics :: [(Var, Value)]
builtinArithmetics =
  [ ("+", cbvProc1 (fmap Number . builtinSum)),
    ("-", cbvProc1 (fmap Number . builtinDiff)),
    ("*", cbvProc1 (fmap Number . builtinProd)),
    ("/", cbvProc1 (fmap Number . builtinDiv)),
    ("div", cbvProc1 (fmap Number . buildinDiv)),
    ("mod", cbvProc1 (fmap Number . buildinMod))
  ]
    ++ builtinNumComparisons

builtinNumComparisons :: [(Var, Value)]
builtinNumComparisons =
  [ ("=", cbvProc1 (fmap Bool . builtinOrd (==))),
    ("<", cbvProc1 (fmap Bool . builtinOrd (<))),
    (">", cbvProc1 (fmap Bool . builtinOrd (>))),
    ("<=", cbvProc1 (fmap Bool . builtinOrd (<=))),
    (">=", cbvProc1 (fmap Bool . builtinOrd (>=)))
  ]

builtinOrd :: (Number -> Number -> Bool) -> [Value] -> Eval Bool
builtinOrd cmp [Number a, Number b] =
  return $ cmp a b
builtinOrd cmp (Number a : Number b : xs) = do
  v <- builtinOrd cmp (Number b : xs)
  return $ v && cmp a b
builtinOrd _ _ = throwError "ord: invalid arguments"

builtinSum :: [Value] -> Eval Number
builtinSum [] = return 0
builtinSum [Number n] = return n
builtinSum (Number a : Number b : xs) =
  builtinSum (Number (a + b) : xs)
builtinSum _ = throwError "+: invalid arguments"

builtinDiff :: [Value] -> Eval Number
builtinDiff [] = throwError "-: not enough arguments"
builtinDiff [Number n] = return (- n)
builtinDiff (Number n : xs) = do
  s <- builtinSum xs
  return (n - s)
builtinDiff _ = throwError "-: invalid arguments"

builtinProd :: [Value] -> Eval Number
builtinProd [] = return 1
builtinProd [Number n] = return n
builtinProd (Number a : Number b : xs) =
  builtinProd (Number (a * b) : xs)
builtinProd _ = throwError "*: invalid arguments"

builtinDiv :: [Value] -> Eval Number
builtinDiv [] = throwError "/: not enough arguments"
builtinDiv [Number n] = return (1 / n)
builtinDiv (Number n : xs) = do
  p <- builtinProd xs
  return (n / p)
builtinDiv _ = throwError "/: invalid arguments"

buildinDiv :: [Value] -> Eval Number
buildinDiv [Number _, Number 0] = throwError "div: undefined for 0"
buildinDiv [Number a, Number b] = fst <$> divAndMod a b
buildinDiv _ = throwError "div: invalid arguments"

buildinMod :: [Value] -> Eval Number
buildinMod [Number _, Number 0] = throwError "mod: undefined for 0"
buildinMod [Number a, Number b] = snd <$> divAndMod a b
buildinMod _ = throwError "mod: invalid arguments"

divAndMod :: Number -> Number -> Eval (Number, Number)
divAndMod _ 0 = throwError "divAndMod: undefined for 0"
divAndMod (Exact x1) (Exact x2) =
  return (Exact q, Exact r)
  where
    (q, r) = x1 `calcDivMod` x2
divAndMod (Ratio x1) (Ratio x2) = x1' `divAndMod` x2'
  where
    r = x1 / x2
    x1' = Exact $ numerator r
    x2' = Exact $ denominator r
divAndMod (Exact x1) x2@(Ratio _) = Ratio (fromIntegral x1) `divAndMod` x2
divAndMod x1@(Ratio _) (Exact x2) = x1 `divAndMod` Ratio (fromIntegral x2)
divAndMod (Inexact x1) (Inexact x2) =
  return (Inexact q, Inexact r)
  where
    (q', r) = x1 `calcDivMod'` x2
    q = fromInteger q'
divAndMod (Exact x1) x2 = Inexact (fromIntegral x1) `divAndMod` x2
divAndMod (Ratio x1) x2 = Inexact (fromRational x1) `divAndMod` x2
divAndMod x1 (Exact x2) = x1 `divAndMod` Inexact (fromIntegral x2)
divAndMod x1 (Ratio x2) = x1 `divAndMod` Inexact (fromRational x2)

-- | Perform integer division on x1 and x2, such that the remainder is always
-- positive
calcDivMod :: (Integral a) => a -> a -> (a, a)
calcDivMod n d =
  if r < 0
    then (q - ds, r + abs d)
    else (q, r)
  where
    q = div n d
    r = n - q * d
    ds = signum d

-- | Generalization of @calcDivMod@ for Real types.
calcDivMod' :: (Real a, Integral b) => a -> a -> (b, a)
calcDivMod' n d =
  if r < 0
    then (q - ds, r + abs d)
    else (q, r)
  where
    q = div' n d
    r = n - fromIntegral q * d
    ds = if d < 0 then -1 else 1

-- | Builtin I/O procedures
builtinIO :: [(Var, Value)]
builtinIO =
  [ ("display", cbvProc1 builtinDisplay),
    ("newline", cbvProc1 builtinNewline)
  ]

-- | Builtin display procedure
builtinDisplay :: [Value] -> Eval Value
builtinDisplay [String s] = liftIO (putStr s) $> Void
builtinDisplay [Symbol s] = liftIO (putStr s) $> Void
builtinDisplay [Char c] = liftIO (putChar c) $> Void
builtinDisplay [Pair (Symbol "quote") (Pair v Empty)] =
  liftIO (putStr "'") >> builtinDisplay [v]
builtinDisplay [Pair (Symbol "quasiquote") (Pair v Empty)] =
  liftIO (putStr "`") >> builtinDisplay [v]
builtinDisplay [Pair (Symbol "unquote") (Pair v Empty)] =
  liftIO (putStr ",") >> builtinDisplay [v]
builtinDisplay [Pair (Symbol "unquote-splicing") (Pair v Empty)] =
  liftIO (putStr ",@") >> builtinDisplay [v]
builtinDisplay [Pair (Symbol "syntax") (Pair v Empty)] =
  liftIO (putStr "#'") >> builtinDisplay [v]
builtinDisplay [Pair (Symbol "quasisyntax") (Pair v Empty)] =
  liftIO (putStr "#`") >> builtinDisplay [v]
builtinDisplay [Pair (Symbol "unsyntax") (Pair v Empty)] =
  liftIO (putStr "#,") >> builtinDisplay [v]
builtinDisplay [Pair (Symbol "unsyntax-splicing") (Pair v Empty)] =
  liftIO (putStr "#,@") >> builtinDisplay [v]
builtinDisplay [Pair car cdr] =
  liftIO (putChar '(')
    >> builtinDisplay [car]
    >> expand cdr
    >> liftIO (putChar ')')
    $> Void
  where
    expand Empty = pure Void
    expand (Pair car' cdr') =
      liftIO (putChar ' ') >> builtinDisplay [car'] >> expand cdr'
    expand d = liftIO (putStr " . ") >> builtinDisplay [d]
builtinDisplay [x] = liftIO (putStr $ show x) $> Void
builtinDisplay _ = throwError "display: invalid arguments"

-- | Builtin newline procedure
builtinNewline :: [Value] -> Eval Value
builtinNewline [] = liftIO (putStrLn "") $> Void
builtinNewline _ = throwError "newline: invalid arguments"

-- | Utility procedures
builtinUtils :: [(Var, Value)]
builtinUtils =
  [ ("call-with-current-continuation", cbvProc builtinCallCc),
    ("call/cc", cbvProc builtinCallCc),
    ("values", cbvProc pure),
    ("call-with-values", cbvProc builtinCallWithValues),
    ("apply", cbvProc builtinApply),
    ("void", cbvProc1 builtinVoid),
    ("error", cbvProc1 builtinError)
  ]

-- | Builtin call-with-current-continuation procedure.
builtinCallCc :: [Value] -> Eval [Value]
builtinCallCc [Procedure p] =
  callCC $ \escape -> do
    l <- alloc (Procedure $ fetchAll >=> escape)
    p [l]
builtinCallCc _ = throwError "call/cc: invalid arguments"

-- | Builtin call-with-values procedure.
builtinCallWithValues :: [Value] -> Eval [Value]
builtinCallWithValues [Procedure p, Procedure c] = p [] >>= allocAll >>= c
builtinCallWithValues _ = throwError "call-with-values: invalid arguments"

-- | Builtin apply procedure.
builtinApply :: [Value] -> Eval [Value]
builtinApply (Procedure p : a : as) = unroll (a :| as) >>= p
  where
    unroll (Empty :| []) = pure []
    unroll (Pair car b :| []) = do v <- alloc car; (:) v <$> unroll (b :| [])
    unroll (v :| vs : vs') = do v' <- alloc v; (:) v' <$> unroll (vs :| vs')
    unroll _ = throwError "apply: invalid arguments"
builtinApply _ = throwError "apply: invalid arguments"

-- | Builtin "void" procedure
builtinVoid :: [Value] -> Eval Value
builtinVoid [] = pure Void
builtinVoid _ = throwError "void: invalid arguments"

-- | Builtin "error" procedure
builtinError :: [Value] -> Eval Value
builtinError (String who : String msg : _) = throwError $ who ++ ": " ++ msg
builtinError _ = throwError "error: invalid arguments"

-- | Debugging procedures
builtinDebug :: [(Var, Value)]
builtinDebug =
  [ ("dump-heap", cbvProc1 builtinDumpHeap),
    ("expand", cbvProc builtinExpand)
  ]

-- | Builtin "dump-heap" procedure
builtinDumpHeap :: [Value] -> Eval Value
builtinDumpHeap [] = get >>= (liftIO . print) >> return Empty
builtinDumpHeap _ = throwError "dump-heap: too many arguments"

-- | Builtin "expand" procedure
builtinExpand :: [Value] -> Eval [Value]
builtinExpand [v] = do
  v' <- case datumFromValue v of
    Nothing -> throwError $ "Invalid syntax: " ++ show v
    Just v' -> pure v'
  expandCtx <- asks $ Map.map (const Variable)
  (ds, _) <- liftEither $ runExpand (expandProgram [v']) expandCtx
  return $ valueFromDatum <$> ds
builtinExpand _ = throwError "expand: invalid arguments"
