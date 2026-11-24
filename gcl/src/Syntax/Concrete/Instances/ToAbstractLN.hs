{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module Syntax.Concrete.Instances.ToAbstractLN where

import Control.Monad (foldM, (<=<))
import Control.Monad.Except (Except, throwError)
import Control.Monad.Reader (ReaderT (..), local, reader)
import Control.Monad.State
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Foldable (Foldable (foldMap'))
import Data.List (partition)
import Data.Text (Text)
import GCL.Common
import GCL.Range (Range, MaybeRanged (..), maybeRangeOf, rangeOf, (<--->), maybeSetRangeOf, maybeSetRange)
import Pretty.Util
  ( PrettyWithRange (prettyWithRange),
    docToText,
    toDoc,
  )
import qualified Syntax.AbstractLN as A
import qualified Syntax.AbstractLN.Operator as A
import Syntax.Common (ArithOp, Name (..), nameToText, TypeOp(..))
import qualified Syntax.Common.Types as C (ArithOp(..), ChainOp(..))
import qualified Syntax.Common.BuiltInOp as A
import Syntax.Concrete.Types
import Syntax.Concrete.Instances.Free ()
import Syntax.Concrete.Instances.Located ()
import Syntax.Common.Instances.Located ()


-- | Typeclass for converting Syntax.Concrete to Syntax.Abstract
class ToAbstract a b | a -> b where
  toAbstract :: a -> AbsM b

instance (ToAbstract a b) => ToAbstract (Maybe a) (Maybe b) where
  toAbstract Nothing = return Nothing
  toAbstract (Just x) = Just <$> toAbstract x

instance (ToAbstract a b, ToAbstract c d) => ToAbstract (Either a c) (Either b d) where
  toAbstract = bitraverse toAbstract toAbstract

instance (ToAbstract a b) => ToAbstract [a] [b] where
  toAbstract = mapM toAbstract

type AbsM a = ReaderT ScopeEnv (StateT HoleCount (Except ScopeError)) a

type ScopeEnv = ([Text], -- types
                 [Text], -- global identifiers in functional sections
                 Bool,   -- allow lookup in the functional section?
                 [(Text, Int)]) -- all the other identifiers

types_in_scope  :: ScopeEnv -> [Text]
types_in_scope (ts, _, _, _) = ts

vars_in_scope :: ScopeEnv -> [(Text, Int)]
vars_in_scope (_, _, _, env) = env

vars_in_ffrag  :: ScopeEnv -> [Text]
vars_in_ffrag (_, _, False, _) = []
vars_in_ffrag (_, fs, True, _) = fs

type HoleCount = Int

data ScopeError =
        VarNotInScope Text (Maybe Range)
      | TypeNotInScope Text (Maybe Range)
      | DuplicatedIdentifiers Text [Maybe Range]
      | ClauseArityMismatch Text [Maybe Range]
      | ParAssignmentMismatch (Maybe Range)
    deriving Show

--------------------------------------------------------------------------------

lookupType :: Text -> AbsM Bool
lookupType t = reader ((t `elem`) . types_in_scope)

lookupFVar :: Text -> AbsM Bool
lookupFVar x = reader ((x `elem`) . vars_in_ffrag)

lookupVar :: Text -> AbsM (Maybe Int)
lookupVar x = reader (lookup x . vars_in_scope)

lookupOrRaise :: Text -> Maybe Range -> AbsM A.Var
lookupOrRaise x l = do
  bound <- lookupVar x
  case bound of
    Just i -> return (A.BName i)   -- local binders shadow globals
    Nothing -> do
      isGlobal <- lookupFVar x
      if isGlobal
        then return (A.FName x)
        else throwError (VarNotInScope x l)

lookupTypeOrRaise :: Text -> Maybe Range -> AbsM ()
lookupTypeOrRaise t l =
  lookupType t >>= \b ->
  if b then return ()
       else throwError (TypeNotInScope t l)

inPureFragment :: AbsM a -> AbsM a
inPureFragment = local setBit
  where setBit :: ScopeEnv -> ScopeEnv
        setBit (ts, fs, _, vs) = (ts, fs, True, vs)

withDefinedTypes :: [Text] -> AbsM a -> AbsM a
withDefinedTypes ts = local ext
  where ext :: ScopeEnv -> ScopeEnv
        ext (_, fs, b, vs) = (ts, fs, b, vs)

withDefinedVals :: [Text] -> AbsM a -> AbsM a
withDefinedVals fs = local ext
  where ext :: ScopeEnv -> ScopeEnv
        ext (ts, _, b, vs) = (ts, fs, b, vs)

withVars :: [Text] -> AbsM a -> AbsM a
withVars vs = local ext
  where ext :: ScopeEnv -> ScopeEnv
        ext (ts, fs, b, _) = (ts, fs, b, zip vs [0..])

withLocalVars :: [Text] -> AbsM a -> AbsM a
withLocalVars xs = local ext
  where ext :: ScopeEnv -> ScopeEnv
        ext (ts, fs, b, vs) = (ts, fs, b, zip (xs ++ map fst vs) [0..])


--------------------------------------------------------------------------------

-- | Program

instance ToAbstract Program A.Program where
  toAbstract (Program defns decls stmts pragmas) = do

      -- collect definitions
     let defs = allDefs defns
     let (tdefs, vdefs) = processDefns defs

     -- A definition without parameters has no pattern to choose a clause by,
     -- so a second clause for it is a duplicate definition.
     noDuplicates [n | ValDefn n [] _ _ <- defs]

     -- All clauses of a definition must take the same number of parameters.
     sameArity [(n, length ps) | ValDefn n ps _ _ <- defs]

     let valNames = map (\(n,_,_) -> n) vdefs
     let ts = allDefinedTypes tdefs

     withDefinedTypes (map (nameToText . fst) ts) (do

       -- collect data constructors
       constrs <- collectAllConstrs tdefs
       let cntrNames = map fst constrs

       -- collect declarations
       let (consts, vars, ps) = processDecls decls
       let (constNames, varNames) = (map fst consts, map fst vars)
       let (glob, assertions) =
              pickGlobalsC (map nameToText constNames) ps

       noDuplicates (cntrNames ++ valNames ++ constNames ++ varNames)

       let pureVars = map nameToText (cntrNames ++ valNames)
       let localVars = map nameToText (reverse (constNames ++ varNames))
       withDefinedVals pureVars $
        withLocalVars localVars (do

         vdefsA <- inPureFragment (desugarDefns vdefs)

         constsA <- zip constNames <$> mapM (toAbstract . snd) consts
         varsA <- zip varNames <$> mapM (toAbstract . snd) vars
         globA <- inPureFragment (toAbstract glob)

         assertionsA <- inPureFragment (toAbstract assertions)
         stmtsA <- toAbstract stmts

         pragmasA <- toAbstract pragmas

         let pre = [A.Assert (A.conjunct assertionsA) Nothing | not (null assertions)]
         return (A.Program ts constrs vdefsA constsA varsA globA (pre ++ stmtsA) pragmasA)
         ))

--------------------------------------------------------------------------------

allDefs :: [DefinitionBlock] -> [Definition]
allDefs = concat . map unDefinitionBlock
  where unDefinitionBlock (DefinitionBlock _ defs _) = defs

       -- processDecls: re-partition declarations into
       --   (a list of constants and their types,
       --    a list of variables and their types,
       --    and all their properties )

--------------------------------------------------------------------------------
-- | Definitions in Functional Fragments

  -- processDefns: re-partition definitions in the funcional
  --   segments into type and value definitions.

processDefns :: [Definition] -> ([(Name, [Name], [TypeDefnCtor])]
                                ,[(Name, Maybe Type, [([Pattern], Expr)])])
processDefns defns =
   let (tdefs, rest) = splitTypeValDefns defns
       vdefs = groupValDefns rest
   in (tdefs, vdefs)

 where
  splitTypeValDefns :: [Definition] -> ([(Name, [Name], [TypeDefnCtor])], [Definition])
  splitTypeValDefns = foldMap'
         ( \d -> case d of
              TypeDefn _ t args _ ctrs -> ([(t, args, sepToList ctrs)], [])
              ValDefnSig {} -> ([], [d])
              ValDefn {} -> ([], [d])
         )

-- | Type Definitions

allDefinedTypes :: [(Name, [Name], [TypeDefnCtor])] -> [(Name, Int)]
allDefinedTypes = map (\(t, args, _) -> (t, length args))

collectAllConstrs :: [(Name, [Name], [TypeDefnCtor])] ->
                    AbsM [(Name, A.Type)]    -- Value Constructors
collectAllConstrs = (return . concat) <=< mapM collectConstrs

   -- Collect constructors of one type, e.g, given:
   --   data T a b = C1 a U | C2 b V
   -- should produce [(C1, ∀ a b . a -> U -> T a b), (C2, ∀ a b . V -> b -> T a b)]
collectConstrs :: (Name, [Name], [TypeDefnCtor]) -> AbsM [(Name, A.Type)]
collectConstrs (tcon, targs, cntrs) = do
  noDuplicates targs
  noDuplicates (map (\(TypeDefnCtor c _) -> c) cntrs)
  mapM genCntr cntrs
 where
  targsT = map nameToText targs

      -- process one constructor
  genCntr :: TypeDefnCtor -> AbsM (Name, A.Type)
  genCntr (TypeDefnCtor c args) =
     (c,) . A.tforall targsT <$>                 -- e.g. ∀ a b ...
        withVars (reverse targsT) (mkCntrType args)      -- put only [a,b] into the environment

      -- generate e,g: a -> U -> T a b
      -- the base case, the "TargetType", is T a b.

  mkCntrType :: [Type] -> AbsM A.Type
  mkCntrType [] = return (mkTargetType [0 .. length targs - 1])
  mkCntrType (t:ts) = A.arr <$> toAbstract t <*> mkCntrType ts

      -- Generate ((T a) b)
  mkTargetType [] = A.tfname (nameToText tcon)
  mkTargetType (i:is) = A.tapp (mkTargetType is) (A.tbname i)


-- | Value Definitions

groupValDefns :: [Definition] -> [(Name, Maybe Type, [([Pattern], Expr)])]
groupValDefns defns = case defns of
  [] -> []
  (ValDefnSig (DeclBase ns _ ty) : ds) ->
    let names = sepToList ns
        (ds1, ds2) = span (elemDefn names) ds
    in map (fetchDefn ty ds1) names ++ groupValDefns ds2
  (d@(ValDefn name _ _ _) : ds) ->
    let (ds1, ds2) = span (sameDefn name) ds
    in (name, Nothing, map unValDefn (d : ds1)) : groupValDefns ds2
  _ -> error "groupValDefns: Patterns exhausted. Shouldn't happen"

 where

   elemDefn names (ValDefn name' _ _ _) = name' `elem` names
   elemDefn _ _ = False

   sameDefn n (ValDefn name' _ _ _) = n == name'
   sameDefn _ _ = False

   fetchDefn ty dfns n = mkDefn . filter (sameDefn n) $ dfns
      where mkDefn ds' = (n, Just ty, map unValDefn ds')

   unValDefn (ValDefn _v ptn _ rhs) = (ptn, rhs)
   unValDefn _ = error "groupValDefns/unValDefn: Patterns exhausted. Shouldn't happen"


desugarDefns :: [(Name, Maybe Type, [([Pattern], Expr)])]
             -> AbsM [(Name, Maybe A.Type, Maybe A.Expr)]
desugarDefns = mapM desugarDefn

desugarDefn :: (Name, Maybe Type, [([Pattern], Expr)])
             -> AbsM (Name, Maybe A.Type, Maybe A.Expr)
desugarDefn (x, t, []) = do t' <- toAbstract t
                            return (x, t', Nothing)
desugarDefn (x, t, [(ptns, body)])              -- one clause
  | Just binders <- mapM isBinder ptns = do     -- all patterns are non-refutable
    noDuplicates (filter notWildcard binders)   -- this also covers the case where ptns = []
    let bindersT = map nameToText binders       -- since mapM f [] = Just []
    t' <- toAbstract t
    ((x, t',) . Just . mkLam bindersT) <$>
        withLocalVars (reverse bindersT) (toAbstract body)
  where
    isBinder (PattParen _ p _) = isBinder p
    isBinder (PattBinder v) = Just v
    isBinder (PattWildcard _) = Just (Name "" Nothing) -- SCM: hack. notWildcard filters Name "" out.
    isBinder _ = Nothing

desugarDefn (x, t, cls) = do
    t' <- toAbstract t
    (x, t', ) . Just .
     mkLam emptyDummies .
       mkCase arity <$>
         withLocalVars emptyDummies (mapM mkCaseClause cls)
  where
    arity = maximum (map (length . fst) cls)
    emptyDummies = replicate arity "_"

    mkCase 0 _ = error "mkCase 0. Shouldn't happen."  -- ruled out by the parameterless-definition check in toAbstract Program
    mkCase n cl =
      -- The first argument belongs to the outermost lambda, at index n - 1.
      A.Case (A.mkTuple (map A.bname (reverse [0..n-1]))) cl
        (maybeRangeOf cl)

    mkCaseClause (ptns, body) = do
      let xs = concat (map extractBinders ptns)
      noDuplicates (filter notWildcard xs)
      let xs' = map nameToText xs
      (,) <$> (A.mkPattTuple <$> mapM toAbstract ptns) <*>
               withLocalVars xs' (toAbstract body)

mkLam :: [Text] -> A.Expr -> A.Expr
mkLam [] body = body
mkLam (x:xs) body = A.Lam x (mkLam xs body) (maybeRangeOf body)

notWildcard :: Name -> Bool
notWildcard (Name "" _) = False  -- Hack: isBinder turns a wildcard parameter into Name "".
notWildcard _ = True


--------------------------------------------------------------------------------
-- | GCL Const and Var Declarations

processDecls :: [Declaration] -> ([(Name, Type)], [(Name, Type)], [Expr])
processDecls [] = ([], [], [])
processDecls (ConstDecl _ (DeclType (DeclBase cs _ ty) p) : decls) =
       let cs' = sepToList cs
           (consts, vars, props) = processDecls decls
       in ( [(c, ty) | c <- cs'] ++ consts, vars,
            maybe props ((: props) . extractProp) p)
processDecls (VarDecl _ (DeclType (DeclBase cs _ ty) p) : decls) =
       let cs' = sepToList cs
           (consts, vars, props) = processDecls decls
       in ( consts, [(c, ty) | c <- cs'] ++ vars,
            maybe props ((: props) . extractProp) p)

extractProp :: DeclProp -> Expr
extractProp (DeclProp _ e _) = e

pickGlobalsC :: [Text] -> [Expr] -> ([Expr], [Expr])
pickGlobalsC consts = partition isConst
  where isConst e = all (`elem` consts) (freeVarsT e)


--------------------------------------------------------------------------------
-- | Statement

withRange :: (Monad m, MaybeRanged a) => (a -> m (Maybe Range -> b)) -> a -> m b
withRange f x = ($ maybeRangeOf x) <$> f x

instance ToAbstract Stmt A.Stmt where
 toAbstract stmt = withRange toAbstract' stmt
  where
    toAbstract' (Skip _) = return A.Skip
    toAbstract' (Abort _) = return A.Abort
    toAbstract' a@(Assign xs _ es) =
      A.Assign <$> handleParAssign (sepToList xs) (sepToList es) (maybeRangeOf a)
    toAbstract' (AAssign x _ i _ _ e) =
      A.AAssign <$> (maybeSetRange l . A.var <$> lookupOrRaise (nameToText x) l)
        <*> toAbstract i <*> toAbstract e
      where l = maybeRangeOf x
    toAbstract' (Assert _ a _) = inPureFragment (A.Assert <$> toAbstract a)
    toAbstract' (LoopInvariant _ a _ _ _ b _) =
      inPureFragment (A.LoopInvariant <$> toAbstract a <*> toAbstract b)
    toAbstract' (Do _ a _) = A.Do <$> toAbstract a
    toAbstract' (If _ a _) = A.If <$> toAbstract a
    toAbstract' (SpecQM _) = error "SpecQM should be digged before calling toAbstract"
    toAbstract' (Spec l xs r) = do
      let text = docToText $ toDoc $ prettyWithRange (map (fmap show) xs)
      return $ const (A.Spec text (rangeOf l <> rangeOf r))
    toAbstract' (Alloc p _ _ _ es _) =
      A.Alloc <$> lookupOrRaise (nameToText p) l
              <*> toAbstract es
      where l = maybeRangeOf p
    toAbstract' (HLookup x _ _ e) =
      A.HLookup <$> lookupOrRaise (nameToText x) l
                <*> toAbstract e
      where l = maybeRangeOf x
    toAbstract' (HMutate _ e1 _ e2) = A.HMutate <$> toAbstract e1 <*> toAbstract e2
    toAbstract' (Dispose _ e) = A.Dispose <$> toAbstract e
    toAbstract' (Block _ _p _) = undefined -- A.Block <$> toAbstract p

    handleParAssign :: [Either Name Hole] -> [Expr] -> Maybe Range
                    -> AbsM [(Either A.Var A.Hole, A.Expr)]
    handleParAssign xs es l
     | length xs /= length es = throwError (ParAssignmentMismatch l)
     | otherwise = mapM asgn (zip xs es)
    asgn (Left (Name x l), e) = do
       v <- lookupOrRaise x l
       e' <- toAbstract e
       return (Left v, e')
    asgn (Right h, e) =
       (,) <$> (Right <$> toAbstract h) <*> toAbstract e

instance ToAbstract GdCmd A.GdCmd where
  toAbstract (GdCmd a _ b) =
     A.GdCmd <$> toAbstract a <*> toAbstract b <*>
       pure (maybeRangeOf a <---> maybeRangeOf b)


--------------------------------------------------------------------------------
-- | Type-Related Stuffs

-- | Type
-- Base types were recognized as TCon (because Base types and TCon are identical at the syntactical level),
-- and to be converted to TBase here.

instance ToAbstract Type A.Type where
  toAbstract t = case t of
    (TBase a) -> A.TLit <$> toAbstract a <*> pure (maybeRangeOf t)
    (TArray _ _a _ b) ->
      maybeSetRangeOf t <$> (A.array <$> toAbstract b)
    (TOp op) -> A.TLit <$> toAbstract op <*> pure (maybeRangeOf t)
    (TData n _) -> do lookupTypeOrRaise (nameToText n) (maybeRangeOf t)
                      return (maybeSetRangeOf t (A.tfname (nameToText n)))
    (TApp l r) ->
      A.TApp
        <$> toAbstract l
        <*> toAbstract r
        <*> pure (maybeRangeOf l <---> maybeRangeOf r)
    (TMetaVar n _) -> let t' = nameToText n in
                      lookupType t' >>= \b ->
                       if b then
                           return (maybeSetRangeOf t (A.tfname t'))
                       else
                           maybeSetRangeOf t . A.tvar <$> lookupOrRaise t' (maybeRangeOf t)
    (TParen _ a _) -> toAbstract a

instance ToAbstract TBase A.TBuiltIn where
  toAbstract (TInt _) = return A.TInt
  toAbstract (TBool _) = return A.TBool
  toAbstract (TChar _) = return A.TChar

instance ToAbstract TypeOp A.TBuiltIn where
  toAbstract (Arrow _) = return A.TArrow

--------------------------------------------------------------------------------
-- | Expressions

instance ToAbstract Expr A.Expr where
 toAbstract (Paren _ e _) = toAbstract e
 toAbstract (Lit l) = A.Lit <$> toAbstract l <*> pure (maybeRangeOf l)
 toAbstract (Var (Name x l)) =
   A.Var <$> lookupOrRaise x l <*> pure l
 toAbstract (Const (Name x l)) =
   A.Var <$> lookupOrRaise x l <*> pure l
 toAbstract (Op op) = A.Lit . A.Op <$> toAbstract op <*> pure (maybeRangeOf op)
 toAbstract (Chain ch) = A.Chain <$> toAbstract ch
 toAbstract e@(Arr arr _ i _) =
   maybeSetRangeOf e <$> (A.arridx <$> toAbstract arr <*> toAbstract i)
 toAbstract e@(App e0 e1) =
   A.App <$> toAbstract e0 <*> toAbstract e1 <*> pure (maybeRangeOf e)
 toAbstract e@(Quant _ op xs _ ran _ bod _) = do
   noDuplicates xs
   A.Quant <$> toAbstractQOp op <*> pure xs'
    <*> withLocalVars (reverse xs') (toAbstract ran)
    <*> withLocalVars (reverse xs') (toAbstract bod)
    <*> pure (maybeRangeOf e)
   where
     xs' = map nameToText xs
     toAbstractQOp :: Either ArithOp Name -> AbsM A.Expr
     toAbstractQOp qop = case qop of
       Left op' -> maybeSetRangeOf op' <$> (A.litOp <$> toAbstract op')
       Right n@(Name _ _) -> toAbstract (Const n)
 toAbstract e@(Case _ m _ cls) =
    A.Case
      <$> toAbstract m
      <*> toAbstract cls
      <*> pure (maybeRangeOf e)
 toAbstract (EHole h) = A.EHole <$> toAbstract h

instance ToAbstract Chain A.Chain where
  toAbstract (Pure e) = A.Pure <$> toAbstract e <*> pure (maybeRangeOf e)
  toAbstract c@(More ch op e) =
     A.More <$> toAbstract ch
            <*> toAbstract op
            <*> toAbstract e
            <*> pure (maybeRangeOf c)

instance ToAbstract Lit A.Lit where
  toAbstract (LitInt a _) = return $ A.Num a
  toAbstract (LitBool a _) = return $ A.Bol a
  toAbstract (LitChar a _) = return $ A.Chr a

instance ToAbstract ArithOp A.Op where
  toAbstract op = return $ case op of
   (C.Implies _) -> A.Implies
   (C.ImpliesU _) -> A.Implies
   (C.Conj _) -> A.Conj
   (C.ConjU _) -> A.Conj
   (C.Disj _) -> A.Disj
   (C.DisjU _) -> A.Disj
   (C.Neg _) -> A.Neg
   (C.NegU _) -> A.Neg
   (C.NegNum _) -> A.NegNum
   (C.Add _) -> A.Add
   (C.Sub _) -> A.Sub
   (C.Mul _) -> A.Mul
   (C.Div _) -> A.Div
   (C.Mod _) -> A.Mod
   (C.Max _) -> A.Max
   (C.Min _) -> A.Min
   (C.Exp _) -> A.Exp
   (C.Hash _) -> A.Hash
   (C.PointsTo _) -> A.PointsTo
   (C.SConj _) -> A.SConj
   (C.SImp _) -> A.SImp

instance ToAbstract C.ChainOp A.Op where
  toAbstract op = return $ case op of
    (C.EQProp _) -> A.EQProp
    (C.EQPropU _) -> A.EQProp
    (C.EQ _) -> A.EQ
    (C.NEQ _) -> A.NEQ
    (C.NEQU _) -> A.NEQ
    (C.LTE _) -> A.LTE
    (C.LTEU _) -> A.LTE
    (C.GTE _) -> A.GTE
    (C.GTEU _) -> A.GTE
    (C.LT _) -> A.LT
    (C.GT _) -> A.GT

instance ToAbstract CaseClause (A.Pattern, A.Expr) where
  toAbstract (CaseClause patt _ body) = do
     noDuplicates xs
     (,) <$> toAbstract patt <*>
             withLocalVars xs' (toAbstract body)
    where xs = extractBinders patt
          xs' = map nameToText xs

instance ToAbstract Pattern A.Pattern where
  toAbstract (PattLit x) =
    A.PattConstructor . Left <$> toAbstract x <*> pure (maybeRangeOf x) <*> pure []
  toAbstract (PattParen _ p _) = toAbstract p
  toAbstract (PattBinder x) = return $ A.PattBinder (nameToText x) (maybeRangeOf x)
  toAbstract (PattWildcard x) = return $ A.PattWildcard (rangeOf x)
  toAbstract (PattConstructor ctor pats) =
    A.PattConstructor (Right (nameToText ctor)) (maybeRangeOf ctor) <$>
        mapM toAbstract pats

instance ToAbstract Hole A.Hole where
  toAbstract (HoleQM _) = error "HoleQM should be digged before calling toAbstract"
  toAbstract (Syntax.Concrete.Types.Hole l xs r) = do
    holeNumber <- countUp
    let text = docToText $ toDoc $ prettyWithRange (map (fmap show) xs)
     in return $ A.Hole text holeNumber (rangeOf l <> rangeOf r)

instance {-# OVERLAPS #-} ToAbstract [Pragma] [A.Pragma] where
  toAbstract =
    foldM
      ( \acc (Pragma _ content _) ->
          case content of
            (Proof proof _ proofText) -> do
              let rTokToText = docToText . toDoc . prettyWithRange . map (fmap show)
              let proof' = rTokToText proof
              let proofText' = rTokToText proofText
              return (A.Proof proof' proofText' : acc)
      )
      []

--

extractBinders :: Pattern -> [Name]
extractBinders (PattLit _) = []
extractBinders (PattParen _ p _) = extractBinders p
extractBinders (PattBinder x) = [x]
extractBinders (PattWildcard _) = []  -- a wildcard binds nothing, as in Syntax.AbstractLN.Scope.extractBinders
extractBinders (PattConstructor _ xs) = xs >>= extractBinders

  -- Given a list of Names, find the first set of duplicate names, if any
  -- This is for error reporting, and we need not find all duplicates.

findDuplicates :: [Name] -> Maybe (Name, [Name])
findDuplicates [] = Nothing
findDuplicates (x:xs) =
    case filter (x==) xs of
      [] -> findDuplicates xs
      ys -> Just (x,ys)

noDuplicates :: [Name] -> AbsM ()
noDuplicates xs =
  case findDuplicates xs of
     Nothing -> return ()
     Just (x, ys) ->  throwError (DuplicatedIdentifiers (nameToText x)
                                    (map maybeRangeOf (x:ys)))

  -- Given the name and parameter count of every clause, report the first
  -- clause of a definition together with the clauses that disagree with it.

sameArity :: [(Name, Int)] -> AbsM ()
sameArity [] = return ()
sameArity ((x, k) : rest) =
  case [y | (y, k') <- rest, y == x, k' /= k] of
    [] -> sameArity rest
    ys -> throwError (ClauseArityMismatch (nameToText x)
                        (map maybeRangeOf (x:ys)))

sepToList :: SepBy s a -> [a]
sepToList (Head x) = [x]
sepToList (Delim x _ xs) = x : sepToList xs

--------------------------------------------------------------------------------

instance (ToAbstract a b) => ToAbstract (SepBy sep a) [b] where
  toAbstract (Head a) = (: []) <$> toAbstract a
  toAbstract (Delim a _ as) = (:) <$> toAbstract a <*> toAbstract as

instance (ToAbstract a b, ToAbstract c d) => ToAbstract (a, c) (b, d) where
  toAbstract (x, y) = (,) <$> toAbstract x <*> toAbstract y
