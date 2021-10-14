module Expand
  ( Binding (..),
    ExpandCtx (..),
    emptyExpandCtx,
    runExpand,
    expandProgram,
  )
where

import Control.Monad (join)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Functor.Identity (Identity (runIdentity))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Datum

type Ident = String

type Transformer = Datum -> Expand Datum

data Binding
  = Variable
  | Syntax Transformer

data MaybeDeferred a
  = Deferred (Expand a)
  | Expanded a

instance Functor MaybeDeferred where
  fmap f (Deferred x) = Deferred (fmap f x)
  fmap f (Expanded x) = Expanded (f x)

instance Applicative MaybeDeferred where
  pure = Expanded
  Deferred f <*> Deferred x = Deferred (f <*> x)
  Deferred f <*> Expanded x = Deferred (f <*> pure x)
  Expanded f <*> Deferred x = Deferred (f <$> x)
  Expanded f <*> Expanded x = Expanded (f x)

instance Monad MaybeDeferred where
  return = Expanded
  Deferred x >>= f = Deferred $ do
    x' <- x
    case f x' of
      Deferred y -> y
      Expanded y -> pure y
  Expanded x >>= f = f x

sync :: MaybeDeferred a -> Expand a
sync (Deferred x) = x
sync (Expanded x) = pure x

newtype ExpandCtx = ExpandCtx
  { ctxEnv :: Map Ident Binding
  }

emptyExpandCtx :: ExpandCtx
emptyExpandCtx = ExpandCtx Map.empty

lookupIdent :: Ident -> ExpandCtx -> Maybe Binding
lookupIdent ident = Map.lookup ident . ctxEnv

bindIdent :: Ident -> Binding -> ExpandCtx -> ExpandCtx
bindIdent ident v = ExpandCtx . Map.insert ident v . ctxEnv

mergeEnv :: Map Ident Binding -> ExpandCtx -> ExpandCtx
mergeEnv a = ExpandCtx . Map.union a . ctxEnv

type Expand a = StateT ExpandCtx (ExceptT String Identity) a

runExpand :: Expand a -> ExpandCtx -> Either String (a, ExpandCtx)
runExpand e env' = runIdentity $ runExceptT $ runStateT e env'

scope :: Expand a -> Expand a
scope e = do
  ctx <- get
  e' <- e
  put ctx
  return e'

expandProgram :: [Datum] -> Expand [Datum]
expandProgram ds = do
  eds <- sequence <$> mapM expandForm ds
  (NonEmpty.toList =<<) <$> sync eds

expandForm :: Datum -> Expand (MaybeDeferred (NonEmpty Datum))
expandForm dat@(List (s@(Lexeme (Sym ident)) : ds)) = do
  pv <- gets (lookupIdent ident)
  e' <- case pv of
    Just (Syntax t) -> Just . Expanded . (:| []) <$> t dat -- Syntax use
    Just Variable -> Just . Expanded . (:| []) <$> expandApplication (s :| ds)
    Nothing -> expandCoreForm dat -- unbound = core form
  case e' of
    Just e'' -> pure e''
    Nothing -> throwError $ "Unbound symbol: " ++ ident
expandForm dat@(Lexeme (Sym ident)) = do
  pv <- gets (lookupIdent ident)
  case pv of
    Just (Syntax t) -> Expanded . (:| []) <$> t dat
    Just Variable -> pure $ Expanded (dat :| [])
    Nothing -> throwError $ "Unbound symbol: " ++ ident
expandForm dat@(Lexeme _) = return $ Expanded (dat :| [])
expandForm (List (ds : ds')) =
  Expanded . (:| []) <$> expandApplication (ds :| ds')
expandForm dat = throwError $ "Invalid syntax: " ++ show dat

expandExpr :: Datum -> Expand Datum
expandExpr d = do
  f <- expandForm d >>= sync
  allExprs <- and <$> mapM isExpression f
  if allExprs
    then return $ makeExpr f
    else throwError $ "Invalid context for definition " ++ show d
  where
    makeExpr (e :| []) = e
    makeExpr (e :| es) = List (Lexeme (Sym "begin") : e : es)

isDefinition :: Datum -> Expand Bool
isDefinition (List [Lexeme (Sym "define"), Lexeme (Sym _), _]) = do
  pv <- gets (lookupIdent "define")
  case pv of
    Just Variable -> pure False
    _ -> pure True
isDefinition (List (Lexeme (Sym "begin") : ds)) = do
  pv <- gets (lookupIdent "begin")
  case pv of
    Just Variable -> pure False
    _ -> and <$> mapM isDefinition ds
isDefinition (List [Lexeme (Sym "define-syntax"), Lexeme (Sym _), _]) = do
  pv <- gets (lookupIdent "define-syntax")
  case pv of
    Just Variable -> pure False
    _ -> pure True
isDefinition _ = pure False

isExpression :: Datum -> Expand Bool
isExpression = fmap not . isDefinition

expandApplication :: NonEmpty Datum -> Expand Datum
expandApplication (operator :| operands) = do
  operator' <- expandExpr operator
  operands' <- mapM expandExpr operands
  pure $ List (operator' : operands')

expandCoreForm :: Datum -> Expand (Maybe (MaybeDeferred (NonEmpty Datum)))
expandCoreForm d@(List (Lexeme (Sym "define-syntax") : _)) =
  Just . Expanded . (:| []) <$> expandDefineSyntax d
expandCoreForm d@(List (Lexeme (Sym "define") : _)) =
  Just . Deferred . fmap (:| []) <$> expandDefine d
expandCoreForm d@(List (Lexeme (Sym "begin") : _)) = Just <$> expandBegin d
expandCoreForm d@(List (Lexeme (Sym "lambda") : _)) =
  Just . Expanded . (:| []) <$> expandLambda d
expandCoreForm d@(List (Lexeme (Sym "if") : _)) =
  Just . Expanded . (:| []) <$> expandIf d
expandCoreForm d@(List (Lexeme (Sym "quote") : _)) =
  Just . Expanded . (:| []) <$> expandQuote d
expandCoreForm d@(List (Lexeme (Sym "set!") : _)) =
  Just . Expanded . (:| []) <$> expandSet d
expandCoreForm _ = return Nothing

expandDefineSyntax :: Datum -> Expand Datum
expandDefineSyntax (List [Lexeme (Sym "define-syntax"), Lexeme (Sym n), e]) = do
  t <- Syntax <$> evalExpandExpr e
  modify (bindIdent n t)
  pure $ List [Lexeme (Sym "void")]
expandDefineSyntax ds = throwError $ "Invalid syntax: " ++ show ds

expandDefine :: Datum -> Expand (Expand Datum)
expandDefine (List [Lexeme (Sym "define"), Lexeme (Sym n), e]) = do
  modify (bindIdent n Variable)
  return $ do
    e' <- expandExpr e
    pure $ List [Lexeme (Sym "define"), Lexeme (Sym n), e']
expandDefine ds = throwError $ "Invalid syntax: " ++ show ds

expandBegin :: Datum -> Expand (MaybeDeferred (NonEmpty Datum))
expandBegin (List (Lexeme (Sym "begin") : d : ds)) =
  fmap join . sequence <$> mapM expandForm (d :| ds)
expandBegin d = throwError $ "Invalid syntax: " ++ show d

expandLambda :: Datum -> Expand Datum
expandLambda (List (Lexeme (Sym "lambda") : formals : body)) = do
  body' <- withFormals formals (expandBody body)
  pure $ List $ Lexeme (Sym "lambda") : formals : body'
expandLambda ds = throwError $ "Invalid syntax: " ++ show ds

withFormals :: Datum -> Expand [Datum] -> Expand [Datum]
withFormals formals e = do
  names <- checkFormals formals
  let localEnv = Map.fromList (asVar <$> names)
  scope (modify (mergeEnv localEnv) >> e)
  where
    asVar n = (n, Variable)

checkFormals :: Datum -> Expand [String]
checkFormals (Lexeme (Sym name)) = pure [name]
checkFormals (List []) = pure []
checkFormals (List (Lexeme (Sym n) : ds)) = do
  names <- checkFormals $ List ds
  return $ n : names
checkFormals (List (p : _)) =
  throwError $ "Invalid formal parameter: " ++ show p
checkFormals (ImproperList l (Sym n)) = do
  names <- checkFormals $ List $ NonEmpty.toList l
  return $ n : names
checkFormals (ImproperList _ a) =
  throwError $ "Invalid formal parameter: " ++ show a
checkFormals d = throwError $ "Invalid formal syntax: " ++ show d

expandBody :: [Datum] -> Expand [Datum]
expandBody [] = pure []
expandBody (f : fs) = do
  f' <- expandExpr f -- TODO: bodies can start with a series of definitions
  fs' <- expandBody fs
  pure $ f' : fs'

expandIf :: Datum -> Expand Datum
expandIf (List [Lexeme (Sym "if"), cond, then', else']) = do
  cond' <- expandExpr cond
  then'' <- expandExpr then'
  else'' <- expandExpr else'
  pure $ List [Lexeme (Sym "if"), cond', then'', else'']
expandIf d = throwError $ "Invalid syntax: " ++ show d

expandQuote :: Datum -> Expand Datum
expandQuote dat@(List [Lexeme (Sym "quote"), _]) = pure dat
expandQuote d = throwError $ "Invalid syntax: " ++ show d

expandSet :: Datum -> Expand Datum
expandSet (List [Lexeme (Sym "set!"), Lexeme (Sym name), value]) = do
  pv <- gets (lookupIdent name)
  case pv of
    Just Variable -> do
      value' <- expandExpr value
      pure $ List [Lexeme (Sym "set!"), Lexeme (Sym name), value']
    _ -> throwError $ "Not a variable: " ++ name
expandSet d = throwError $ "Invalid syntax: " ++ show d

evalExpandExpr :: Datum -> Expand Transformer
evalExpandExpr dat@(List (Lexeme (Sym "syntax-rules") : List lits : rules)) = do
  lits' <- mapM getSymbol lits
  makeSyntaxRules lits' rules
  where
    getSymbol :: Datum -> Expand String
    getSymbol (Lexeme (Sym s)) = pure s
    getSymbol _ = throwError $ "Invalid syntax: " ++ show dat
evalExpandExpr d = throwError $ "Invalid syntax: " ++ show d

makeSyntaxRules :: [String] -> [Datum] -> Expand Transformer
makeSyntaxRules _ _ = throwError "syntax-rules unimplemented"
