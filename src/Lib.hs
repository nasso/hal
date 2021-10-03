module Lib
  ( withStdLib,
    withBuiltins,
    withFormStr,
    withProgramStr,
    withFile,
    withFiles,
  )
where

import Control.Applicative
import Datum
import Eval
import My.Control.Monad.Trans.ErrorT
import My.Control.Monad.Trans.IO
import My.Control.Monad.Trans.ParserT
import Program

parseAst :: Program.Parser a -> String -> Maybe a
parseAst p s = do
  (ds, []) <- runParserT (many datum) s
  (ast, []) <- runParserT p ds
  return ast

withFormStr :: String -> Eval (Maybe Value -> a) -> Eval a
withFormStr s e = case parseAst Program.form s of
  Nothing -> throwError "Syntax error"
  Just f -> evalForm f e

withProgramStr :: String -> Eval a -> Eval a
withProgramStr s e = case parseAst Program.program s of
  Nothing -> throwError "Syntax error"
  Just p -> evalProgram p e

withFile :: FilePath -> Eval a -> Eval a
withFile f e = do
  src <- liftIO $ readFile f
  withProgramStr src e

withFiles :: [FilePath] -> Eval a -> Eval a
withFiles = foldr ((.) . withFile) id

withStdLib :: Eval a -> Eval a
withStdLib = withBuiltins . withFile "lang/std.scm"

withBuiltins :: Eval a -> Eval a
withBuiltins = defineAll builtins

procedure :: ([Value] -> Eval Value) -> Value
procedure = Closure emptyEnv

builtins :: [(Var, Value)]
builtins = builtinList ++ builtinPredicates ++ builtinArithmetics

builtinList :: [(Var, Value)]
builtinList =
  [ ("cons", procedure (fmap Datum . builtinCons)),
    ("car", procedure (fmap Datum . builtinCar)),
    ("cdr", procedure (fmap Datum . builtinCdr))
  ]

builtinCons :: [Value] -> Eval Datum
builtinCons [Datum a, Datum b] = return $ Cons a b
builtinCons _ = throwError "cons: wrong number of arguments"

builtinCar :: [Value] -> Eval Datum
builtinCar [Datum (Cons car _)] = return car
builtinCar _ = throwError "car: invalid arguments"

builtinCdr :: [Value] -> Eval Datum
builtinCdr [Datum (Cons _ cdr)] = return cdr
builtinCdr _ = throwError "cdr: invalid arguments"

builtinPredicates :: [(Var, Value)]
builtinPredicates =
  [ ("eqv?", procedure (fmap (Datum . Datum.Bool) . builtinEqv)),
    ("eq?", procedure (fmap (Datum . Datum.Bool) . builtinEq)),
    ("boolean?", procedure (fmap (Datum . Datum.Bool) . builtinIsBool)),
    ("number?", procedure (fmap (Datum . Datum.Bool) . builtinIsNumber)),
    ("symbol?", procedure (fmap (Datum . Datum.Bool) . builtinIsSymbol)),
    ("char?", procedure (fmap (Datum . Datum.Bool) . builtinIsChar)),
    ("string?", procedure (fmap (Datum . Datum.Bool) . builtinIsString)),
    ("pair?", procedure (fmap (Datum . Datum.Bool) . builtinIsPair)),
    ("null?", procedure (fmap (Datum . Datum.Bool) . builtinIsNull))
  ]

builtinIsBool :: [Value] -> Eval Bool
builtinIsBool [Datum (Datum.Bool _)] = return True
builtinIsBool [_] = return False
builtinIsBool _ = throwError "boolean?: invalid arguments"

builtinIsNumber :: [Value] -> Eval Bool
builtinIsNumber [Datum (Datum.Number _)] = return True
builtinIsNumber [_] = return False
builtinIsNumber _ = throwError "number?: invalid arguments"

builtinIsSymbol :: [Value] -> Eval Bool
builtinIsSymbol [Datum (Symbol _)] = return True
builtinIsSymbol [_] = return False
builtinIsSymbol _ = throwError "symbol?: invalid arguments"

builtinIsChar :: [Value] -> Eval Bool
builtinIsChar [Datum (Datum.Char _)] = return True
builtinIsChar [_] = return False
builtinIsChar _ = throwError "char?: invalid arguments"

builtinIsString :: [Value] -> Eval Bool
builtinIsString [Datum (Datum.String _)] = return True
builtinIsString [_] = return False
builtinIsString _ = throwError "string?: invalid arguments"

builtinIsPair :: [Value] -> Eval Bool
builtinIsPair [Datum (Cons _ _)] = return True
builtinIsPair [_] = return False
builtinIsPair _ = throwError "pair?: invalid arguments"

builtinIsNull :: [Value] -> Eval Bool
builtinIsNull [Datum Empty] = return True
builtinIsNull [_] = return False
builtinIsNull _ = throwError "null?: invalid arguments"

builtinEq :: [Value] -> Eval Bool
builtinEq [Datum Empty, Datum Empty] = return True
builtinEq [Datum (Datum.Char c), Datum (Datum.Char c')] = return $ c == c'
builtinEq [Datum (Datum.Number n), Datum (Datum.Number n')] = return $ n == n'
builtinEq [Datum (Datum.Bool b), Datum (Datum.Bool b')] = return $ b == b'
builtinEq [Datum (Datum.String s), Datum (Datum.String s')] = return $ s == s'
builtinEq [Datum (Symbol s), Datum (Symbol s')] = return $ s == s'
builtinEq [_, _] = return False
builtinEq _ = throwError "eq?: invalid arguments"

builtinEqv :: [Value] -> Eval Bool
builtinEqv [Datum (Cons a b), Datum (Cons a' b')] = do
  aEqv <- builtinEqv [Datum a, Datum a']
  bEqv <- builtinEqv [Datum b, Datum b']
  return $ aEqv && bEqv
builtinEqv [a, b] = builtinEq [a, b]
builtinEqv _ = throwError "eqv?: invalid arguments"

builtinArithmetics :: [(Var, Value)]
builtinArithmetics =
  [ ("+", procedure (fmap (Datum . Datum.Number) . builtinSum)),
    ("-", procedure (fmap (Datum . Datum.Number) . builtinDiff)),
    ("*", procedure (fmap (Datum . Datum.Number) . builtinProd)),
    ("/", procedure (fmap (Datum . Datum.Number) . builtinDiv)),
    ("div", procedure (fmap (Datum . Datum.Number) . builtinQuot)),
    ("mod", procedure (fmap (Datum . Datum.Number) . builtinMod))
  ]
    ++ builtinNumComparisons

builtinNumComparisons :: [(Var, Value)]
builtinNumComparisons =
  [ ("=", procedure (fmap (Datum . Datum.Bool) . builtinOrd (==))),
    ("<", procedure (fmap (Datum . Datum.Bool) . builtinOrd (<))),
    (">", procedure (fmap (Datum . Datum.Bool) . builtinOrd (>))),
    ("<=", procedure (fmap (Datum . Datum.Bool) . builtinOrd (<=))),
    (">=", procedure (fmap (Datum . Datum.Bool) . builtinOrd (>=)))
  ]

builtinOrd :: (Double -> Double -> Bool) -> [Value] -> Eval Bool
builtinOrd cmp [Datum (Datum.Number a), Datum (Datum.Number b)] =
  return $ cmp a b
builtinOrd cmp (Datum (Datum.Number a) : Datum (Datum.Number b) : xs) = do
  v <- builtinOrd cmp (Datum (Datum.Number b) : xs)
  return $ v && cmp a b
builtinOrd _ _ = throwError "ord: invalid arguments"

builtinSum :: [Value] -> Eval Double
builtinSum [] = return 0
builtinSum [Datum (Datum.Number n)] = return n
builtinSum (Datum (Datum.Number a) : Datum (Datum.Number b) : xs) =
  builtinSum (Datum (Datum.Number (a + b)) : xs)
builtinSum _ = throwError "+: invalid arguments"

builtinDiff :: [Value] -> Eval Double
builtinDiff [] = throwError "-: not enough arguments"
builtinDiff [Datum (Datum.Number n)] = return (- n)
builtinDiff (Datum (Datum.Number n) : xs) = do
  s <- builtinSum xs
  return (n - s)
builtinDiff _ = throwError "-: invalid arguments"

builtinProd :: [Value] -> Eval Double
builtinProd [] = return 1
builtinProd [Datum (Datum.Number n)] = return n
builtinProd (Datum (Datum.Number a) : Datum (Datum.Number b) : xs) =
  builtinProd (Datum (Datum.Number (a * b)) : xs)
builtinProd _ = throwError "*: invalid arguments"

builtinDiv :: [Value] -> Eval Double
builtinDiv [] = throwError "/: not enough arguments"
builtinDiv [Datum (Datum.Number n)] = return (1 / n)
builtinDiv (Datum (Datum.Number n) : xs) = do
  p <- builtinProd xs
  return (n / p)
builtinDiv _ = throwError "/: invalid arguments"

builtinQuot :: [Value] -> Eval Double
builtinQuot [Datum (Datum.Number a), Datum (Datum.Number b)] =
  return $ fromIntegral $ a' `quot` b'
  where
    a' = truncate a :: Integer
    b' = truncate b :: Integer
builtinQuot _ = throwError "div: invalid arguments"

builtinMod :: [Value] -> Eval Double
builtinMod [Datum (Datum.Number a), Datum (Datum.Number b)] =
  return $ fromIntegral $ a' `rem` b'
  where
    a' = truncate a :: Integer
    b' = truncate b :: Integer
builtinMod _ = throwError "mod: invalid arguments"
