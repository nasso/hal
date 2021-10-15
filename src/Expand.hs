module Expand
  ( Binding (..),
    ExpandCtx,
    defaultExpandCtx,
    runExpand,
    expandProgram,
    setVars,
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
import Data.Set (Set)
import qualified Data.Set as Set
import Datum

type Ident = String

type Transformer = Datum -> Expand (MaybeDeferred (NonEmpty Datum))

data EValue
  = EProc (Datum -> Expand EValue)
  | Syntax Transformer

data Binding
  = Variable
  | EValue EValue

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

defaultExpandCtx :: ExpandCtx
defaultExpandCtx =
  ExpandCtx $
    Map.fromList
      [ ("define", EValue $ Syntax defineForm),
        ("begin", EValue $ Syntax beginForm),
        ("lambda", EValue $ Syntax lambdaForm),
        ("if", EValue $ Syntax ifForm),
        ("quote", EValue $ Syntax quoteForm),
        ("set!", EValue $ Syntax setForm),
        ("define-syntax", EValue $ Syntax defineSyntaxForm),
        ("syntax-rules", EValue $ EProc (fmap Syntax . syntaxRulesProc))
      ]

lookupIdent :: Ident -> ExpandCtx -> Maybe Binding
lookupIdent ident = Map.lookup ident . ctxEnv

bindIdent :: Ident -> Binding -> ExpandCtx -> ExpandCtx
bindIdent ident v = ExpandCtx . Map.insert ident v . ctxEnv

mergeEnv :: Map Ident Binding -> ExpandCtx -> ExpandCtx
mergeEnv a = ExpandCtx . Map.union a . ctxEnv

setVars :: [Ident] -> ExpandCtx -> ExpandCtx
setVars [] ctx = ctx {ctxEnv = Map.filter notVar $ ctxEnv ctx}
  where
    notVar Variable = False
    notVar _ = True
setVars (v : vs) ctx = bindIdent v Variable (setVars vs ctx)

getBinding :: Ident -> Expand Binding
getBinding ident = do
  v <- gets (lookupIdent ident)
  case v of
    Just v' -> pure v'
    Nothing -> throwError $ "Unbound identifier: " ++ ident

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
-- (<keyword> <datum> ...)
expandForm dat@(List (s@(Lexeme (Sym ident)) : ds)) = do
  pv <- getBinding ident
  -- might be a procedure application
  case pv of
    EValue (Syntax syn) -> syn dat
    Variable -> Expanded . (:| []) <$> expandApplication (s :| ds)
    _ -> throwError $ "Invalid syntax: " ++ show dat

-- (<keyword> <datum> ... . <datum>)
expandForm dat@(ImproperList (Lexeme (Sym ident) :| _) _) = do
  pv <- getBinding ident
  -- could only be a syntactic form
  case pv of
    EValue (Syntax syn) -> syn dat
    _ -> throwError $ "Invalid syntax: " ++ show dat

-- <keyword>
expandForm dat@(Lexeme (Sym ident)) = do
  pv <- getBinding ident
  -- might be a variable reference
  case pv of
    EValue (Syntax syn) -> syn dat
    Variable -> pure $ Expanded (dat :| []) -- just a variable reference
    _ -> throwError $ "Invalid syntax: " ++ show dat

-- other lexemes are just passed through (e.g. numbers, strings)
expandForm dat@(Lexeme _) = pure $ Expanded (dat :| [])
-- any list might be a procedure application
expandForm (List (ds : ds')) =
  Expanded . (:| []) <$> expandApplication (ds :| ds')
-- everything else is a syntax error
expandForm dat = throwError $ "Invalid syntax: " ++ show dat

expandApplication :: NonEmpty Datum -> Expand Datum
expandApplication (operator :| operands) = do
  operator' <- expandExpr operator
  operands' <- mapM expandExpr operands
  pure $ List (operator' : operands')

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

defineForm :: Transformer
defineForm (List [_, Lexeme (Sym n), e]) =
  modify (bindIdent n Variable)
    >> pure
      ( Deferred $ do
          e' <- expandExpr e
          pure $ List [Lexeme (Sym "define"), Lexeme (Sym n), e'] :| []
      )
defineForm ds = throwError $ "Invalid syntax: " ++ show ds

beginForm :: Transformer
beginForm (List (_ : d : ds)) =
  fmap join . sequence <$> mapM expandForm (d :| ds)
beginForm d = throwError $ "Invalid syntax: " ++ show d

lambdaForm :: Transformer
lambdaForm (List (_ : formals : body)) = do
  body' <- withFormals formals (expandBody body)
  pure . Expanded $ List (Lexeme (Sym "lambda") : formals : body') :| []
lambdaForm ds = throwError $ "Invalid syntax: " ++ show ds

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
  -- TODO: bodies can start with a series of definitions
  -- in that case, they must be replaced with the equivalent of a letrec* form
  f' <- expandExpr f
  fs' <- expandBody fs
  pure $ f' : fs'

ifForm :: Transformer
ifForm (List [_, cond, then', else']) = do
  cond' <- expandExpr cond
  then'' <- expandExpr then'
  else'' <- expandExpr else'
  pure . Expanded $ List [Lexeme (Sym "if"), cond', then'', else''] :| []
ifForm d = throwError $ "Invalid syntax: " ++ show d

quoteForm :: Transformer
quoteForm dat@(List [Lexeme (Sym "quote"), _]) = pure . Expanded $ dat :| []
quoteForm d = throwError $ "Invalid syntax: " ++ show d

setForm :: Transformer
setForm dat@(List [_, Lexeme (Sym name), v]) = do
  binding <- getBinding name
  case binding of
    EValue (Syntax syn) -> syn dat -- (set! <keyword> <datum>) macro
    Variable -> do
      v' <- expandExpr v
      pure . Expanded $ List [Lexeme (Sym "set!"), Lexeme (Sym name), v'] :| []
    _ -> throwError $ "Invalid syntax: " ++ show dat
setForm d = throwError $ "Invalid syntax: " ++ show d

defineSyntaxForm :: Transformer
defineSyntaxForm (List [Lexeme (Sym "define-syntax"), Lexeme (Sym n), e]) = do
  -- TODO: expand e before evaluating it
  e' <- evalExpandExpr e
  case e' of
    s@(Syntax _) ->
      modify (bindIdent n $ EValue s)
        >> pure (Expanded $ List [Lexeme (Sym "void")] :| [])
    _ -> throwError $ "Invalid syntax: " ++ show e
defineSyntaxForm ds = throwError $ "Invalid syntax: " ++ show ds

evalExpandExpr :: Datum -> Expand EValue
evalExpandExpr dat@(List (Lexeme (Sym ident) : _)) = do
  binding <- getBinding ident
  case binding of
    EValue (EProc p) -> p dat
    _ -> throwError $ "Invalid syntax: " ++ show dat
evalExpandExpr d = throwError $ "Invalid syntax: " ++ show d

type StringSet = Set String

data SyntaxRule = SyntaxRule Pattern Template

parseSyntaxRule :: StringSet -> Datum -> Expand SyntaxRule
parseSyntaxRule lits (List [List (Lexeme (Sym _) : ps), template]) = do
  (pat, vars) <- parsePattern lits $ List (Lexeme (Sym "_") : ps)
  template' <- parseTemplate vars template
  _ <- throwError $ show template'
  pure $ SyntaxRule pat template'
parseSyntaxRule _ d = throwError $ "Invalid syntax: " ++ show d

checkedUnions :: [StringSet] -> Expand StringSet
checkedUnions [] = pure Set.empty
checkedUnions (s1 : ss) = do
  s2 <- checkedUnions ss
  if Set.disjoint s1 s2
    then pure $ Set.union s1 s2
    else
      throwError $
        "Duplicate pattern variable(s): "
          ++ show (Set.toList $ Set.intersection s1 s2)

data Pattern
  = PAny
  | PVar String
  | PConst Constant
  | PList [Pattern]
  | PImproper [Pattern] Pattern
  | PVarList [Pattern] Pattern [Pattern]
  | PVarImproper [Pattern] Pattern [Pattern] Pattern
  deriving (Show)

parsePattern :: StringSet -> Datum -> Expand (Pattern, StringSet)
parsePattern _ (Lexeme (Sym "...")) = throwError "Unexpected ellipsis"
parsePattern _ (Lexeme (Sym "_")) = pure (PAny, Set.empty)
parsePattern lits (Lexeme (Sym s))
  | Set.member s lits = pure (PConst $ Sym s, Set.empty)
  | otherwise = pure (PVar s, Set.singleton s)
parsePattern _ (Lexeme c) = pure (PConst c, Set.empty)
parsePattern _ (List []) = pure (PList [], Set.empty)
parsePattern lits (List (pat : Lexeme (Sym "...") : pats)) = do
  (pat', patvs) <- parsePattern lits pat
  (ps, vs) <- unzip <$> mapM (parsePattern lits) pats
  uvs <- checkedUnions $ patvs : vs
  pure (PVarList [] pat' ps, uvs)
parsePattern lits (List (p : ps)) = do
  (p', pvs) <- parsePattern lits p
  (ps', vs) <- parsePattern lits (List ps)
  uvs <- checkedUnions [pvs, vs]
  case ps' of
    PList pats -> pure (PList (p' : pats), uvs)
    PVarList pats pat pats' -> pure (PVarList (p' : pats) pat pats', uvs)
    _ -> error "parsePattern: unexpected pattern type"
parsePattern lits (ImproperList (pat :| []) cdrp) = do
  (pat', patvs) <- parsePattern lits pat
  (cdrp', cdrvs) <- parsePattern lits $ Lexeme cdrp
  uvs <- checkedUnions [patvs, cdrvs]
  pure (PImproper [pat'] cdrp', uvs)
parsePattern lits (ImproperList (pat :| Lexeme (Sym "...") : pats) cdrp) = do
  (pat', patvs) <- parsePattern lits pat
  (ps, vs) <- unzip <$> mapM (parsePattern lits) pats
  (cdrp', cdrvs) <- parsePattern lits (Lexeme cdrp)
  uvs <- checkedUnions $ patvs : cdrvs : vs
  pure (PVarImproper [] pat' ps cdrp', uvs)
parsePattern lits (ImproperList (p :| ps : pss) cdrp) = do
  (p', pvs) <- parsePattern lits p
  (ps', vs) <- parsePattern lits (ImproperList (ps :| pss) cdrp)
  uvs <- checkedUnions [pvs, vs]
  case ps' of
    PImproper pats cdrp' -> pure (PImproper (p' : pats) cdrp', uvs)
    PVarImproper pats pat pats' cdrp' ->
      pure (PVarImproper (p' : pats) pat pats' cdrp', uvs)
    _ -> error "parsePattern: unexpected pattern type"

data SubTemplate = SubTemplate Template Word deriving (Show)

data Template
  = TVar String
  | TConst Constant
  | TList [SubTemplate]
  | TImproper [SubTemplate] Template
  deriving (Show)

parseTemplate :: StringSet -> Datum -> Expand Template
parseTemplate pvars (Lexeme (Sym s)) | Set.member s pvars = pure (TVar s)
parseTemplate _ (Lexeme l) = pure (TConst l)
parseTemplate pvars (List sts) = TList <$> parseSubTemplates pvars sts
parseTemplate pvars (ImproperList sts t) =
  TImproper
    <$> parseSubTemplates pvars (NonEmpty.toList sts)
    <*> parseTemplate pvars (Lexeme t)

parseSubTemplates :: StringSet -> [Datum] -> Expand [SubTemplate]
parseSubTemplates _ [] = pure []
parseSubTemplates pvars (st : sts) = do
  st' <- parseTemplate pvars st
  let (n, sts') = takeEllipses sts
  sts'' <- parseSubTemplates pvars sts'
  pure $ SubTemplate st' n : sts''

takeEllipses :: [Datum] -> (Word, [Datum])
takeEllipses (Lexeme (Sym "...") : sts) =
  (n + 1, sts')
  where
    (n, sts') = takeEllipses sts
takeEllipses sts = (0, sts)

parseLiterals :: [Datum] -> StringSet -> Expand StringSet
parseLiterals [] s = pure s
parseLiterals (Lexeme (Sym "...") : _) _ =
  throwError "A syntax literal cannot be an ellipsis or an underscore."
parseLiterals (Lexeme (Sym "_") : _) _ =
  throwError "A syntax literal cannot be an ellipsis or an underscore."
parseLiterals (Lexeme (Sym l) : ls) s = parseLiterals ls (Set.insert l s)
parseLiterals (dat : _) _ = throwError $ "Not a literal: " ++ show dat

syntaxRulesProc :: Datum -> Expand Transformer
syntaxRulesProc (List (_ : List lits : rules)) = do
  lits' <- parseLiterals lits Set.empty
  rules' <- mapM (parseSyntaxRule lits') rules
  makeSyntaxRulesTransformer rules'
syntaxRulesProc d = throwError $ "Invalid syntax: " ++ show d

makeSyntaxRulesTransformer :: [SyntaxRule] -> Expand Transformer
makeSyntaxRulesTransformer [] =
  pure $ \d -> throwError $ "Invalid syntax: " ++ show d
makeSyntaxRulesTransformer _ = throwError "Unsupported."
