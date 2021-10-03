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
import Datum (datum)
import Eval
import My.Control.Monad.Trans.ErrorT
import My.Control.Monad.Trans.IO
import My.Control.Monad.Trans.ParserT
import Program (Parser, Var, form, program)

parseAst :: Program.Parser a -> String -> Maybe a
parseAst p s = do
  (ds, []) <- runParserT (many datum) s
  (ast, []) <- runParserT p ds
  return ast

withFormStr :: String -> (Maybe Value -> Eval a) -> Eval a
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
  [ ("cons", procedure builtinPair),
    ("car", procedure builtinCar),
    ("cdr", procedure builtinCdr)
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
  [ ("eqv?", procedure (fmap Bool . builtinEqv)),
    ("eq?", procedure (fmap Bool . builtinEq)),
    ("boolean?", procedure (fmap Bool . builtinIsBool)),
    ("number?", procedure (fmap Bool . builtinIsNumber)),
    ("symbol?", procedure (fmap Bool . builtinIsSymbol)),
    ("char?", procedure (fmap Bool . builtinIsChar)),
    ("string?", procedure (fmap Bool . builtinIsString)),
    ("pair?", procedure (fmap Bool . builtinIsPair)),
    ("null?", procedure (fmap Bool . builtinIsNull))
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

builtinEq :: [Value] -> Eval Bool
builtinEq [Empty, Empty] = return True
builtinEq [Char c, Char c'] = return $ c == c'
builtinEq [Number n, Number n'] = return $ n == n'
builtinEq [Bool b, Bool b'] = return $ b == b'
builtinEq [String s, String s'] = return $ s == s'
builtinEq [Symbol s, Symbol s'] = return $ s == s'
builtinEq [_, _] = return False
builtinEq _ = throwError "eq?: invalid arguments"

builtinEqv :: [Value] -> Eval Bool
builtinEqv [Pair a b, Pair a' b'] = do
  aEqv <- builtinEqv [a, a']
  bEqv <- builtinEqv [b, b']
  return $ aEqv && bEqv
builtinEqv [a, b] = builtinEq [a, b]
builtinEqv _ = throwError "eqv?: invalid arguments"

builtinArithmetics :: [(Var, Value)]
builtinArithmetics =
  [ ("+", procedure (fmap Number . builtinSum)),
    ("-", procedure (fmap Number . builtinDiff)),
    ("*", procedure (fmap Number . builtinProd)),
    ("/", procedure (fmap Number . builtinDiv)),
    ("div", procedure (fmap Number . builtinQuot)),
    ("mod", procedure (fmap Number . builtinMod))
  ]
    ++ builtinNumComparisons

builtinNumComparisons :: [(Var, Value)]
builtinNumComparisons =
  [ ("=", procedure (fmap Bool . builtinOrd (==))),
    ("<", procedure (fmap Bool . builtinOrd (<))),
    (">", procedure (fmap Bool . builtinOrd (>))),
    ("<=", procedure (fmap Bool . builtinOrd (<=))),
    (">=", procedure (fmap Bool . builtinOrd (>=)))
  ]

builtinOrd :: (Double -> Double -> Bool) -> [Value] -> Eval Bool
builtinOrd cmp [Number a, Number b] =
  return $ cmp a b
builtinOrd cmp (Number a : Number b : xs) = do
  v <- builtinOrd cmp (Number b : xs)
  return $ v && cmp a b
builtinOrd _ _ = throwError "ord: invalid arguments"

builtinSum :: [Value] -> Eval Double
builtinSum [] = return 0
builtinSum [Number n] = return n
builtinSum (Number a : Number b : xs) =
  builtinSum (Number (a + b) : xs)
builtinSum _ = throwError "+: invalid arguments"

builtinDiff :: [Value] -> Eval Double
builtinDiff [] = throwError "-: not enough arguments"
builtinDiff [Number n] = return (- n)
builtinDiff (Number n : xs) = do
  s <- builtinSum xs
  return (n - s)
builtinDiff _ = throwError "-: invalid arguments"

builtinProd :: [Value] -> Eval Double
builtinProd [] = return 1
builtinProd [Number n] = return n
builtinProd (Number a : Number b : xs) =
  builtinProd (Number (a * b) : xs)
builtinProd _ = throwError "*: invalid arguments"

builtinDiv :: [Value] -> Eval Double
builtinDiv [] = throwError "/: not enough arguments"
builtinDiv [Number n] = return (1 / n)
builtinDiv (Number n : xs) = do
  p <- builtinProd xs
  return (n / p)
builtinDiv _ = throwError "/: invalid arguments"

builtinQuot :: [Value] -> Eval Double
builtinQuot [Number a, Number b] =
  return $ fromIntegral $ a' `quot` b'
  where
    a' = truncate a :: Integer
    b' = truncate b :: Integer
builtinQuot _ = throwError "div: invalid arguments"

builtinMod :: [Value] -> Eval Double
builtinMod [Number a, Number b] =
  return $ fromIntegral $ a' `rem` b'
  where
    a' = truncate a :: Integer
    b' = truncate b :: Integer
builtinMod _ = throwError "mod: invalid arguments"
