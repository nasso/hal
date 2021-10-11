module Lib
  ( withBaseLib,
    withFormStr,
    withFormStr',
    withProgramStr,
    withProgramStr',
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
import Data.Stream (LineStream, ListStream (..), Stream (Pos), makeLineStream)
import Datum (datum)
import Expand
import Program
import TreeWalker

type LineParseError = ParseError (Pos LineStream)

parseLines ::
  ParserT LineStream Identity a ->
  String ->
  Either LineParseError a
parseLines p s = case runIdentity $ runParserT (p <* eof) $ makeLineStream s of
  NoParse e -> Left e
  Parsed v _ _ -> Right v

parseList :: ParserT (ListStream i) Identity a -> [i] -> Eval a
parseList p s = case runIdentity $ runParserT (p <* eof) $ ListStream s 0 of
  NoParse e -> throwError $ show e
  Parsed v _ _ -> return v

parseAst :: Program.Parser a -> String -> Eval (Either LineParseError a)
parseAst p s = handleParse $ parseLines (many datum <* eof) s
  where
    handleParse (Left e) = pure $ Left e
    handleParse (Right ds) = do
      expandCtx <- asks $ Map.map (const Variable)
      (ds', _) <- liftEither $ runExpand (expandProgram ds) expandCtx
      Right <$> parseList p ds'

withFormStr' :: String -> (Either LineParseError [Value] -> Eval ()) -> Eval ()
withFormStr' s k = do
  f <- parseAst Program.form s
  case f of
    Left e -> k $ Left e
    Right v -> evalForm v (k . Right)

withProgramStr' ::
  String ->
  (Maybe LineParseError -> Eval ()) ->
  Eval ()
withProgramStr' s k = do
  f <- parseAst Program.program s
  case f of
    Left e -> k $ Just e
    Right v -> evalProgram v $ k Nothing

withFormStr :: String -> ([Value] -> Eval ()) -> Eval ()
withFormStr s k = withFormStr' s go
  where
    go (Left e) = throwError $ show e
    go (Right v) = k v

withProgramStr :: String -> Eval () -> Eval ()
withProgramStr s k = withProgramStr' s go
  where
    go Nothing = k
    go (Just e) = throwError $ show e

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
