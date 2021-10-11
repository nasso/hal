module Lib
  ( StrEvalResult,
    StrEvalError (..),
    withBaseLib,
    withStr,
    withStr',
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
import Data.Stream (LineStream, ListStream (ListStream), Stream (Pos), makeLineStream)
import Datum (Datum, datum)
import Expand
import Syntax
import TreeWalker

type LineParseError = ParseError (Pos LineStream)

data StrEvalError
  = CantParse LineParseError
  | SyntaxError String
  deriving (Show)

type StrEvalResult a = Either StrEvalError a

doExpand :: [Datum] -> Eval (StrEvalResult [Datum])
doExpand tokens = do
  ctx <- asks $ Map.map (const Variable)
  case runExpand (expandProgram tokens) ctx of
    Left err -> pure $ Left $ SyntaxError err
    Right (ast, _) -> pure $ Right ast

parseAst :: String -> Eval (StrEvalResult Program)
parseAst s =
  case runIdentity $ runParserT (many datum <* eof) $ makeLineStream s of
    NoParse e -> pure $ Left $ CantParse e
    Parsed v _ _ -> (>>= expand) <$> doExpand v
  where
    expand v =
      case runIdentity $ runParserT Syntax.readProgram (ListStream v 0) of
        NoParse e -> Left $ SyntaxError $ show e
        Parsed ast _ _ -> Right ast

withStr' :: String -> (StrEvalResult [Value] -> Eval ()) -> Eval ()
withStr' s k = parseAst s >>= go
  where
    go (Left e) = k $ Left e
    go (Right v) = evalProgram v (k . Right)

withStr :: String -> ([Value] -> Eval ()) -> Eval ()
withStr s k = withStr' s go
  where
    go (Left e) = throwError $ show e
    go (Right v) = k v

withFile :: FilePath -> Eval () -> Eval ()
withFile f e = do
  src <- liftIO $ readFile f
  withStr src $ const e

withFiles :: [FilePath] -> Eval () -> Eval ()
withFiles = foldr ((.) . withFile) id

withBaseLib :: Eval () -> Eval ()
withBaseLib = withBuiltins . withFile "lang/base.scm"

withBuiltins :: Eval () -> Eval ()
withBuiltins = defineAll baseProcedures
