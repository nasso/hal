module Main where

import Control.Monad (when)
import Grammar.Datum (display)
import Lib (EvalError, eval)
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
    Just (Args files i) -> runWithArgs files (i || null files)
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

runWithArgs :: [String] -> Bool -> IO ()
runWithArgs [] False = return ()
runWithArgs [] True = repl
runWithArgs (path : paths) i = do
  src <- readFile path
  case eval src of
    Left e -> ePrint e
    Right v -> putStr path >> putStr " -> " >> putStrLn (display v)
  runWithArgs paths i

repl :: IO ()
repl = do
  line <- prompt >> getLine
  when (line /= "exit") $
    case eval line of
      Left e -> ePrint e
      Right v -> putStrLn $ display v
      >> repl

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
