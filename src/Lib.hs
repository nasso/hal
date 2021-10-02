module Lib (exec', loadStdLib, loadBuiltins, evalString, loadFile) where

import Control.Applicative
import Control.Monad
import Datum
import Eval
import My.Control.Monad.Trans.ParserT

exec' :: Eval a -> Either EvalError (EvalResult a)
exec' e = case eval e [] newContext of
  Left err -> Left err
  Right (r, _, _) -> Right r

evalString :: String -> Eval (Maybe Value)
evalString s = case runParserT (many datum) s of
  Nothing -> evalError SyntaxError
  Just (ds, "") -> reval program ds
  Just (_, extra) -> raise $ String $ "Extra text: " ++ extra

loadFile :: FilePath -> IO (Eval ())
loadFile f = void . evalString <$> readFile f

loadStdLib :: IO (Eval ())
loadStdLib = do
  std <- loadFile "lang/std.scm"
  return $ loadBuiltins >> std

loadBuiltins :: Eval ()
loadBuiltins = mapM_ (uncurry define) builtins

builtins :: [(String, Value)]
builtins = builtinList ++ builtinPredicates ++ builtinArithmetics

builtinList :: [(String, Value)]
builtinList =
  [ ("cons", Procedure (fmap Datum . builtinCons)),
    ("car", Procedure (fmap Datum . builtinCar)),
    ("cdr", Procedure (fmap Datum . builtinCdr))
  ]

builtinCons :: [Value] -> Eval Datum
builtinCons [Datum a, Datum b] = return $ Cons a b
builtinCons _ = raise $ String "cons: wrong number of arguments"

builtinCar :: [Value] -> Eval Datum
builtinCar [Datum (Cons car _)] = return car
builtinCar _ = raise $ String "car: invalid arguments"

builtinCdr :: [Value] -> Eval Datum
builtinCdr [Datum (Cons _ cdr)] = return cdr
builtinCdr _ = raise $ String "cdr: invalid arguments"

builtinPredicates :: [(String, Value)]
builtinPredicates =
  [ ("eqv?", Procedure (fmap (Datum . Bool) . builtinEqv)),
    ("eq?", Procedure (fmap (Datum . Bool) . builtinEq)),
    ("boolean?", Procedure (fmap (Datum . Bool) . builtinIsBool)),
    ("number?", Procedure (fmap (Datum . Bool) . builtinIsNumber)),
    ("symbol?", Procedure (fmap (Datum . Bool) . builtinIsSymbol)),
    ("char?", Procedure (fmap (Datum . Bool) . builtinIsChar)),
    ("string?", Procedure (fmap (Datum . Bool) . builtinIsString)),
    ("pair?", Procedure (fmap (Datum . Bool) . builtinIsPair)),
    ("null?", Procedure (fmap (Datum . Bool) . builtinIsNull))
  ]

builtinIsBool :: [Value] -> Eval Bool
builtinIsBool [Datum (Bool _)] = return True
builtinIsBool [_] = return False
builtinIsBool _ = raise $ String "boolean?: invalid arguments"

builtinIsNumber :: [Value] -> Eval Bool
builtinIsNumber [Datum (Number _)] = return True
builtinIsNumber [_] = return False
builtinIsNumber _ = raise $ String "number?: invalid arguments"

builtinIsSymbol :: [Value] -> Eval Bool
builtinIsSymbol [Datum (Symbol _)] = return True
builtinIsSymbol [_] = return False
builtinIsSymbol _ = raise $ String "symbol?: invalid arguments"

builtinIsChar :: [Value] -> Eval Bool
builtinIsChar [Datum (Char _)] = return True
builtinIsChar [_] = return False
builtinIsChar _ = raise $ String "char?: invalid arguments"

builtinIsString :: [Value] -> Eval Bool
builtinIsString [Datum (String _)] = return True
builtinIsString [_] = return False
builtinIsString _ = raise $ String "string?: invalid arguments"

builtinIsPair :: [Value] -> Eval Bool
builtinIsPair [Datum (Cons _ _)] = return True
builtinIsPair [_] = return False
builtinIsPair _ = raise $ String "pair?: invalid arguments"

builtinIsNull :: [Value] -> Eval Bool
builtinIsNull [Datum Empty] = return True
builtinIsNull [_] = return False
builtinIsNull _ = raise $ String "null?: invalid arguments"

builtinEq :: [Value] -> Eval Bool
builtinEq [Datum Empty, Datum Empty] = return True
builtinEq [Datum (Char c), Datum (Char c')] = return $ c == c'
builtinEq [Datum (Number n), Datum (Number n')] = return $ n == n'
builtinEq [Datum (Bool b), Datum (Bool b')] = return $ b == b'
builtinEq [Datum (String s), Datum (String s')] = return $ s == s'
builtinEq [Datum (Symbol s), Datum (Symbol s')] = return $ s == s'
builtinEq [_, _] = return False
builtinEq _ = raise $ String "eq?: invalid arguments"

builtinEqv :: [Value] -> Eval Bool
builtinEqv [Datum (Cons a b), Datum (Cons a' b')] = do
  aEqv <- builtinEqv [Datum a, Datum a']
  bEqv <- builtinEqv [Datum b, Datum b']
  return $ aEqv && bEqv
builtinEqv [a, b] = builtinEq [a, b]
builtinEqv _ = raise $ String "eqv?: invalid arguments"

builtinArithmetics :: [(String, Value)]
builtinArithmetics =
  [ ("+", Procedure (fmap (Datum . Number) . builtinSum)),
    ("-", Procedure (fmap (Datum . Number) . builtinDiff)),
    ("*", Procedure (fmap (Datum . Number) . builtinProd)),
    ("/", Procedure (fmap (Datum . Number) . builtinDiv)),
    ("div", Procedure (fmap (Datum . Number) . builtinQuot)),
    ("mod", Procedure (fmap (Datum . Number) . builtinMod))
  ]
    ++ builtinNumComparisons

builtinNumComparisons :: [(String, Value)]
builtinNumComparisons =
  [ ("=", Procedure (fmap (Datum . Bool) . builtinOrd (==))),
    ("<", Procedure (fmap (Datum . Bool) . builtinOrd (<))),
    (">", Procedure (fmap (Datum . Bool) . builtinOrd (>))),
    ("<=", Procedure (fmap (Datum . Bool) . builtinOrd (<=))),
    (">=", Procedure (fmap (Datum . Bool) . builtinOrd (>=)))
  ]

builtinOrd :: (Double -> Double -> Bool) -> [Value] -> Eval Bool
builtinOrd cmp [Datum (Number a), Datum (Number b)] = return $ cmp a b
builtinOrd cmp (Datum (Number a) : Datum (Number b) : xs) = do
  v <- builtinOrd cmp (Datum (Number b) : xs)
  return $ v && cmp a b
builtinOrd _ _ = raise $ String "ord: invalid arguments"

builtinSum :: [Value] -> Eval Double
builtinSum [] = return 0
builtinSum [Datum (Number n)] = return n
builtinSum (Datum (Number a) : Datum (Number b) : xs) =
  builtinSum (Datum (Number (a + b)) : xs)
builtinSum _ = raise $ String "+: invalid arguments"

builtinDiff :: [Value] -> Eval Double
builtinDiff [] = raise $ String "-: not enough arguments"
builtinDiff [Datum (Number n)] = return (- n)
builtinDiff (Datum (Number n) : xs) = do
  s <- builtinSum xs
  return (n - s)
builtinDiff _ = raise $ String "-: invalid arguments"

builtinProd :: [Value] -> Eval Double
builtinProd [] = return 1
builtinProd [Datum (Number n)] = return n
builtinProd (Datum (Number a) : Datum (Number b) : xs) =
  builtinProd (Datum (Number (a * b)) : xs)
builtinProd _ = raise $ String "*: invalid arguments"

builtinDiv :: [Value] -> Eval Double
builtinDiv [] = raise $ String "/: not enough arguments"
builtinDiv [Datum (Number n)] = return (1 / n)
builtinDiv (Datum (Number n) : xs) = do
  p <- builtinProd xs
  return (n / p)
builtinDiv _ = raise $ String "/: invalid arguments"

builtinQuot :: [Value] -> Eval Double
builtinQuot [Datum (Number a), Datum (Number b)] =
  return $ fromIntegral $ a' `quot` b'
  where
    a' = truncate a :: Integer
    b' = truncate b :: Integer
builtinQuot _ = raise $ String "div: invalid arguments"

builtinMod :: [Value] -> Eval Double
builtinMod [Datum (Number a), Datum (Number b)] =
  return $ fromIntegral $ a' `rem` b'
  where
    a' = truncate a :: Integer
    b' = truncate b :: Integer
builtinMod _ = raise $ String "mod: invalid arguments"
