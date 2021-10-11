module Lib
  ( withBaseLib,
    withFormStr,
    withProgramStr,
    withFile,
    withFiles,
  )
where

import BaseLib.Procedure
import Control.Monad.Except.Class
import Control.Monad.IO.Class
import Control.Monad.Parser.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Parser
import Data.Functor.Identity (Identity (runIdentity))
import qualified Data.Map.Strict as Map
import Data.Stream (LineStream, ListStream (..), makeLineStream)
import Datum (datum)
import Expand
import Program
import TreeWalker

parseLines :: ParserT LineStream Identity a -> String -> Eval a
parseLines p s = case runIdentity $ runParserT (p <* eof) $ makeLineStream s of
  NoParse e -> throwError $ show e
  Parsed v _ _ -> return v

parseList :: ParserT (ListStream i) Identity a -> [i] -> Eval a
parseList p s = case runIdentity $ runParserT (p <* eof) $ ListStream s 0 of
  NoParse e -> throwError $ show e
  Parsed v _ _ -> return v

parseAst :: Program.Parser a -> String -> Eval a
parseAst p s = do
  ds <- parseLines (many datum <* eof) s
  expandCtx <- asks $ Map.map (const Variable)
  (ds', _) <- liftEither $ runExpand (expandProgram ds) expandCtx
  parseList p ds'

withFormStr :: String -> ([Value] -> Eval ()) -> Eval ()
withFormStr s e = do
  f <- parseAst Program.form s
  evalForm f e

withProgramStr :: String -> Eval () -> Eval ()
withProgramStr s e = do
  p <- parseAst Program.program s
  evalProgram p e

withFile :: FilePath -> Eval () -> Eval ()
withFile f e = do
  src <- liftIO $ readFile f
  withProgramStr src e

withFiles :: [FilePath] -> Eval () -> Eval ()
withFiles = foldr ((.) . withFile) id

withBaseLib :: Eval () -> Eval ()
withBaseLib = withBuiltins . withFile "lang/base.scm"

withBuiltins :: Eval () -> Eval ()
withBuiltins = defineAll baseProcedures
