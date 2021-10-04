module Main where

import Control.Monad
import Eval
import Lib
import My.Control.Monad.Trans.ExceptT
import My.Control.Monad.Trans.IO
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hFlush, hPrint, hPutStr, hPutStrLn, stderr, stdout)

data Args
  = Args
      { sourceFiles :: [String],
        interactive :: Bool
      }
  | Help

main :: IO ()
main = getArgs >>= parseArgs defaultArgs >>= handleArgs

defaultArgs :: Args
defaultArgs = Args {sourceFiles = [], interactive = False}

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
  r <- runEval (vm files (i || null files))
  case r of
    Left err -> exitWithMessage err
    Right _ -> return ()

printUsage :: IO ()
printUsage = do
  name <- getProgName
  putStrLn "USAGE"
  putStrLn $ "  " ++ name ++ " [files...] [-i]"
  putStrLn $ "  " ++ name ++ " [-i]"
  putStrLn $ "  " ++ name ++ " -h"
  putStrLn $ "  " ++ name ++ " --help"

vm :: [FilePath] -> Bool -> Eval ()
vm files i = withStdLib $ withFiles files $ when i repl

repl :: Eval ()
repl = do
  line <- liftIO $ prompt >> getLine
  if line == ":quit"
    then return ()
    else evalAndPrint line

evalAndPrint :: String -> Eval ()
evalAndPrint s = withFormStr s displayAndLoop `catchError` displayError
  where
    displayAndLoop Nothing = repl
    displayAndLoop (Just v) = liftIO (print v) >> repl
    displayError msg = liftIO (ePutStrLn msg) >> repl

prompt :: IO ()
prompt = putStr "> " >> hFlush stdout

ePutStr, ePutStrLn :: String -> IO ()
ePutStr = hPutStr stderr
ePutStrLn = hPutStrLn stderr

ePrint :: Show a => a -> IO ()
ePrint = hPrint stderr

exitWithMessage :: String -> IO ()
exitWithMessage msg = ePutStrLn msg >> exitWith (ExitFailure 84)
