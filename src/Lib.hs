module Lib
  ( withBaseLib,
    withFormStr,
    withProgramStr,
    withFile,
    withFiles,
  )
where

import BaseLib.Procedure
import Control.Applicative
import Control.Monad.MyTrans
import qualified Data.Map.Strict as Map
import Datum (datum)
import Expand
import Program
import TreeWalker

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just x) = Right x
maybeToEither x Nothing = Left x

parseAst :: Program.Parser a -> String -> Eval a
parseAst p s = do
  ds <- liftEither $ maybeToEither "Syntax error" $ parse (many datum <* eof) s
  expandCtx <- asks $ Map.map (const Variable)
  (ds', _) <- liftEither $ runExpand (expandProgram ds) expandCtx
  liftEither $ maybeToEither "Core syntax error" $ parse p ds'
  where
    parse p' s' = fst <$> runParserT (p' <* eof) s'

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
