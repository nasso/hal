module Main where

import Control.Monad
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Parser (ParseError (ParseError))
import Data.Stream.TextLines (TextPos (TextPos))
import Lib
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hFlush, hPrint, hPutStr, hPutStrLn, isEOF, stderr, stdout)
import TreeWalker

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
handleArgs (Just (Args files i)) = runEval (vm files (i || null files)) k
  where
    k (Left err) = exitWithMessage err
    k (Right _) = return ()

printUsage :: IO ()
printUsage = do
  name <- getProgName
  putStrLn "USAGE"
  putStrLn $ "  " ++ name ++ " [files...] [-i]"
  putStrLn $ "  " ++ name ++ " [-i]"
  putStrLn $ "  " ++ name ++ " -h"
  putStrLn $ "  " ++ name ++ " --help"

vm :: [FilePath] -> Bool -> Eval ()
vm files i =
  withBaseLib $
    withFiles files $
      \vs -> liftIO (displayAll vs) >> when i startRepl

safeGetLine :: (() -> Eval a) -> String -> Eval String
safeGetLine exit p =
  do
    end <- liftIO $ putStr p >> hFlush stdout >> isEOF
    if end
      then "" <$ exit ()
      else liftIO getLine

startRepl :: Eval ()
startRepl =
  callCC $
    \exit ->
      define "exit" (Procedure $ const $ exit ()) $
        repl (safeGetLine exit) ""

repl :: (String -> Eval String) -> String -> Eval ()
repl getln prev =
  let contLine "" = getln "> "
      contLine prev' = (++) prev' . (:) '\n' <$> getln ".. "
      displayError msg = liftIO (ePutStrLn msg) >> repl getln ""
   in contLine prev >>= \line ->
        withStr' line (printLoop (repl getln) line) `catchError` displayError

printLoop :: (String -> Eval ()) -> String -> EvalResult [Value] -> Eval ()
printLoop loop _ (Right vs) = liftIO (displayAll vs) >> loop ""
printLoop _ _ (Left (SyntaxError err)) = throwError err
printLoop loop s (Left (CantParse e@(ParseError p _)))
  | isAtEnd p = loop s
  | otherwise = throwError $ show e
  where
    allLines = lines s
    lastLineNum = length allLines - 1
    lastLineCol = length $ last allLines
    isAtEnd (TextPos l c _) = l == lastLineNum && c == lastLineCol

displayAll :: [Value] -> IO ()
displayAll (Void : vs') = displayAll vs'
displayAll (v : vs') = print v >> displayAll vs'
displayAll [] = pure ()

ePutStr :: String -> IO ()
ePutStr = hPutStr stderr

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

ePrint :: Show a => a -> IO ()
ePrint = hPrint stderr

exitWithMessage :: String -> IO ()
exitWithMessage msg = ePutStrLn msg >> exitWith (ExitFailure 84)
