module Main where

import Control.Monad (void)
import Lib (Context, EvalError, newContext, sEval)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hFlush, hPrint, hPutStr, hPutStrLn, stderr, stdout)

data Args
  = Args
      { sourceFiles :: [String],
        interactive :: Bool
      }
  | Help

defaultArgs :: Args
defaultArgs = Args {sourceFiles = [], interactive = False}

main :: IO ()
main = do
  args <- getArgs >>= parseArgs defaultArgs
  case args of
    Just (Args files i) -> void $ runWithArgs newContext files (i || null files)
    Just Help -> printUsage
    Nothing -> exitWithMessage "Invalid arguments."

printUsage :: IO ()
printUsage = do
  name <- getProgName
  putStrLn "USAGE"
  putStrLn $ "  " ++ name ++ " [files...] [-i]"
  putStrLn $ "  " ++ name ++ " [-i]"
  putStrLn $ "  " ++ name ++ " -h"
  putStrLn $ "  " ++ name ++ " --help"

exitWithMessage :: String -> IO ()
exitWithMessage msg = ePutStrLn msg >> exitWith (ExitFailure 84)

parseArgs :: Args -> [String] -> IO (Maybe Args)
parseArgs _ ("-h" : _) = return $ Just Help
parseArgs _ ("--help" : _) = return $ Just Help
parseArgs Help _ = return $ Just Help
parseArgs (Args files _) ("-i" : xs) =
  parseArgs (Args {sourceFiles = files, interactive = True}) xs
parseArgs (Args files i) (file : xs) =
  parseArgs (Args {sourceFiles = files ++ [file], interactive = i}) xs
parseArgs args _ = return $ Just args

runWithArgs :: Context -> [String] -> Bool -> IO Context
runWithArgs c [] False = return c
runWithArgs c [] True = repl c
runWithArgs c (path : paths) i = do
  src <- readFile path
  case sEval c src of
    Left e -> ePrint e >> return c
    Right (_, c') -> runWithArgs c' paths i

repl :: Context -> IO Context
repl c = do
  line <- prompt >> getLine
  if line == "exit"
    then return c
    else case sEval c line of
      Left e -> ePrint e >> repl c
      Right (Just v, c') -> print v >> repl c'
      Right (_, c') -> repl c'

prompt :: IO ()
prompt = putStr "> " >> flushStdout

flushStdout :: IO ()
flushStdout = hFlush stdout

ePutStr :: String -> IO ()
ePutStr = hPutStr stderr

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

ePrint :: EvalError -> IO ()
ePrint = hPrint stderr
