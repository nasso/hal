module Main where

import Eval
import Lib
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

printUsage :: IO ()
printUsage = do
  name <- getProgName
  putStrLn "USAGE"
  putStrLn $ "  " ++ name ++ " [files...] [-i]"
  putStrLn $ "  " ++ name ++ " [-i]"
  putStrLn $ "  " ++ name ++ " -h"
  putStrLn $ "  " ++ name ++ " --help"

parseArgs :: Args -> [String] -> IO (Maybe Args)
parseArgs _ ("-h" : _) = return $ Just Help
parseArgs _ ("--help" : _) = return $ Just Help
parseArgs Help _ = return $ Just Help
parseArgs (Args files _) ("-i" : xs) =
  parseArgs (Args {sourceFiles = files, interactive = True}) xs
parseArgs (Args files i) (file : xs) =
  parseArgs (Args {sourceFiles = files ++ [file], interactive = i}) xs
parseArgs args _ = return $ Just args

handleArgs :: Maybe Args -> IO ()
handleArgs Nothing = exitWithMessage "Invalid arguments."
handleArgs (Just Help) = printUsage
handleArgs (Just (Args files i)) = do
  std <- loadStdLib
  runFiles <- sequence_ <$> mapM loadFile files
  repl' <- if i || null files then repl else return $ return Nothing
  case exec' (std >> runFiles >> repl') of
    Left e -> ePrint e >> exitWithMessage "An error has occured."
    Right r -> print r

repl :: IO (Eval (Maybe Value))
repl = do
  line <- prompt >> getLine
  return $ evalString line

prompt :: IO ()
prompt = putStr "> " >> hFlush stdout

ePutStr, ePutStrLn :: String -> IO ()
ePutStr = hPutStr stderr
ePutStrLn = hPutStrLn stderr

ePrint :: Show a => a -> IO ()
ePrint = hPrint stderr

exitWithMessage :: String -> IO ()
exitWithMessage msg = ePutStrLn msg >> exitWith (ExitFailure 84)

main :: IO ()
main = getArgs >>= parseArgs defaultArgs >>= handleArgs
