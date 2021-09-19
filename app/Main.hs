module Main where

import Control.Monad (when)
import Grammar (datum)
import Parsing (Parser (parse))
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hFlush, hPutStrLn, stderr, stdout)

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
exitWithMessage msg = hPutStrLn stderr msg >> exitWith (ExitFailure 84)

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
  case parse datum src of
    Just (v, []) -> print v
    Just (v, r) -> print v >> putStr "Unparsed input: " >> putStrLn r
    Nothing -> hPutStrLn stderr "Syntax error!"
    >> runWithArgs paths i

repl :: IO ()
repl = do
  line <- prompt >> getLine
  when (line /= "exit") $
    case parse datum line of
      Just (v, []) -> print v
      Just (v, r) -> print v >> putStr "Unparsed input: " >> putStrLn r
      Nothing -> hPutStrLn stderr "Syntax error!"
      >> repl

prompt :: IO ()
prompt = putStr "> " >> flushStdout

flushStdout :: IO ()
flushStdout = hFlush stdout
