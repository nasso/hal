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
import Control.Monad.Trans.Parser
import Data.Functor.Identity (Identity (runIdentity))
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

doExpand :: [Datum] -> (StrEvalResult [Datum] -> Eval r) -> Eval r
doExpand tokens k = do
  ctx <- getExpandCtx
  case runExpand (expandProgram tokens) ctx of
    Left err -> k $ Left $ SyntaxError err
    Right (ast, ctx') -> withExpandCtx ctx' $ k $ Right ast

parseAst :: String -> (StrEvalResult Program -> Eval r) -> Eval r
parseAst s k =
  case runIdentity $ runParserT (many datum <* eof) $ makeLineStream s of
    NoParse e -> k $ Left $ CantParse e
    Parsed v _ _ -> doExpand v expand
  where
    expand (Left e) = k $ Left e
    expand (Right v) =
      case runIdentity $ runParserT Syntax.readProgram (ListStream v 0) of
        NoParse e -> k $ Left $ SyntaxError $ show e
        Parsed ast _ _ -> k $ Right ast

withStr' :: String -> (StrEvalResult [Value] -> Eval ()) -> Eval ()
withStr' s k = parseAst s go
  where
    go (Left e) = k $ Left e
    go (Right v) = evalProgram v (k . Right)

withStr :: String -> ([Value] -> Eval ()) -> Eval ()
withStr s k = withStr' s go
  where
    go (Left e) = throwError $ show e
    go (Right v) = k v

withFile :: FilePath -> ([Value] -> Eval ()) -> Eval ()
withFile f k = do
  src <- liftIO $ readFile f
  withStr src k

withFiles :: [FilePath] -> ([Value] -> Eval ()) -> Eval ()
withFiles [] k = k []
withFiles [f] k = withFile f k
withFiles (f : fs) k = withFile f $ const $ withFiles fs k

withBaseLib :: Eval () -> Eval ()
withBaseLib = withBuiltins . withFile "lang/base.scm" . const

withBuiltins :: Eval () -> Eval ()
withBuiltins = defineAll baseProcedures
