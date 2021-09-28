module Main where

import Control.Monad (void)
import Eval
import Grammar.Datum (datum)
import Parsing (Parser (parse))
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

execute :: String -> Eval Value
execute s = case parse datum s of
  Just (d, "") -> eval d
  _ -> throw SyntaxError

runWithArgs :: Context -> [String] -> Bool -> IO ()
runWithArgs _ [] False = return ()
runWithArgs c [] True = repl c
runWithArgs c (path : paths) i = do
  src <- readFile path
  case apply (execute src) c of
    Left e -> ePrint e
    Right (_, c') -> runWithArgs c' paths i

repl :: Context -> IO ()
repl c = do
  line <- prompt >> getLine
  if line == "exit"
    then return ()
    else case apply (execute line) c of
      Left e -> ePrint e >> repl c
      Right (d, c') -> print d >> repl c'

prompt :: IO ()
prompt = putStr "> " >> hFlush stdout

ePutStr :: String -> IO ()
ePutStr = hPutStr stderr

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

ePrint :: EvalError -> IO ()
ePrint = hPrint stderr
