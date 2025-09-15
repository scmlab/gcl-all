{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module GCL.Common where

import Control.Monad.RWS (RWST (..))
import Control.Monad.State (StateT (..))
import Data.Loc.Range (Range)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set
  ( Set,
    (\\),
  )
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics
import Syntax.Abstract
import Syntax.Common.Types

data Index l = Index (Name l) | Hole Range deriving (Eq, Show, Ord)

data TypeInfo l
  = TypeDefnCtorInfo (Type l)
  | ConstTypeInfo (Type l)
  | VarTypeInfo (Type l)
  deriving (Eq, Show, Generic)

toTypeEnv :: [(Index l, TypeInfo l)] -> TypeEnv l
toTypeEnv infos =
  ( \(index, info) ->
      case info of
        TypeDefnCtorInfo ty -> (index, ty)
        ConstTypeInfo ty -> (index, ty)
        VarTypeInfo ty -> (index, ty)
  )
    <$> infos

type TypeEnv l = [(Index l, Type l)]

type KindEnv l = [KindItem l]

data KindItem l
  = KindAnno (Name l) (Kind l)
  | UnsolvedUni (Name l)
  | SolvedUni (Name l) (Kind l)
  deriving (Show)

-- get a fresh variable
class (Monad m) => Fresh m where
  fresh :: m Text
  fresh = freshPre (Text.pack "")

  freshPre :: Text -> m Text

freshName :: (Fresh m) => Text -> l -> m (Name l)
freshName prefix l = Name <$> freshPre prefix <*> pure l

freshName' :: (Fresh m) => Text -> m (Name ())
freshName' prefix = freshName prefix ()

freshNames :: (Fresh m) => [Text] -> m [Name ()]
freshNames = mapM freshName'

class Counterous m where
  countUp :: m Int

instance {-# OVERLAPPABLE #-} (Monad m, Counterous m) => Fresh m where
  fresh = Text.pack . ("?m_" ++) . show <$> countUp
  freshPre prefix =
    Text.pack
      . ("?" ++)
      . (Text.unpack prefix ++)
      . ("_" ++)
      . show
      <$> countUp

type FreshState = Int

initFreshState :: FreshState
initFreshState = 0

type Subs a l = Map (Name l) a

type Env a l = Map (Name l) a

emptySubs :: Ord l => Subs a l
emptySubs = mempty

emptyEnv :: Ord l => Env a l
emptyEnv = mempty

freeMetaVars :: Ord l => Type l -> Set (Name l)
freeMetaVars (TBase _ _) = mempty
freeMetaVars (TArray _ t _) = freeMetaVars t
freeMetaVars (TTuple _) = mempty
freeMetaVars (TFunc l r _) = freeMetaVars l <> freeMetaVars r
freeMetaVars (TOp _) = mempty
freeMetaVars (TData _ _) = mempty
freeMetaVars (TApp l r _) = freeMetaVars l <> freeMetaVars r
freeMetaVars (TVar _ _) = mempty
freeMetaVars (TMetaVar n _) = Set.singleton n

-- A class of types for which we may compute their free variables.
class Free a l where
  freeVars :: a -> Set (Name l)
  -- freeVarsT :: a -> Set Text -- FIXME: cannot unify types
  -- freeVarsT = Set.map nameToText . freeVars

occurs :: (Ord l, Free a l) => Name l -> a -> Bool
occurs n x = n `Set.member` freeVars x

instance (Ord l, Free a l) => Free (Subs a l) l where
  freeVars = Set.unions . Map.map freeVars

instance {-# OVERLAPPABLE #-} (Ord l, Free a l) => Free [a] l where
  freeVars l = foldMap freeVars l

instance (Ord l, Free a l, Free b l) => Free (a, b) l where
  freeVars (x, y) = freeVars x <> freeVars y

instance (Ord l, Free a l, Free b l, Free c l) => Free (a, b, c) l where
  freeVars (x, y, z) = freeVars x <> freeVars y <> freeVars z

instance (Ord l, Free a l) => Free (Maybe a) l where
  freeVars = maybe mempty freeVars

instance Ord l => Free (Type l) l where
  freeVars (TBase _ _) = mempty
  freeVars (TArray _ t _) = freeVars t
  freeVars (TTuple _) = mempty
  freeVars (TFunc l r _) = freeVars l <> freeVars r
  freeVars (TData _ _) = mempty
  freeVars (TOp _) = mempty
  freeVars (TApp l r _) = freeVars l <> freeVars r
  freeVars (TVar x _) = Set.singleton x
  freeVars (TMetaVar n _) = Set.singleton n

instance {-# OVERLAPS #-} Ord l => Free (TypeEnv l) l where
  freeVars env = foldMap freeVars $ Map.elems $ Map.fromList env

instance Ord l => Free (Expr l) l where
  freeVars (Var x _) = Set.singleton x
  freeVars (Const x _) = Set.singleton x
  freeVars (Op _) = mempty
  freeVars (Chain chain) = freeVars chain
  freeVars (Lit _ _) = mempty
  freeVars (App e1 e2 _) = freeVars e1 <> freeVars e2
  freeVars (Func _ clauses _) = Set.unions (fmap freeVars clauses)
  freeVars (Lam x e _) = freeVars e \\ Set.singleton x
  freeVars (Tuple xs) = Set.unions (map freeVars xs)
  freeVars (Quant op xs range term _) =
    (freeVars op <> freeVars range <> freeVars term) \\ Set.fromList xs
  freeVars (RedexKernel _ _ fv _) = fv
  freeVars (RedexShell _ e) = freeVars e
  freeVars (ArrIdx e1 e2 _) = freeVars e1 <> freeVars e2
  freeVars (ArrUpd e1 e2 e3 _) = freeVars e1 <> freeVars e2 <> freeVars e3
  freeVars (Case e clauses _) = freeVars e <> Set.unions (map freeVars clauses)

instance Ord l => Free (Chain l) l where
  freeVars (Pure expr _) = freeVars expr
  freeVars (More chain _op expr _) = freeVars chain <> freeVars expr

instance Ord l => Free (FuncClause l) l where
  freeVars (FuncClause patterns expr) = freeVars expr \\ Set.unions (map freeVars patterns)

instance Ord l => Free (CaseClause l) l where
  freeVars (CaseClause patt expr) = freeVars expr \\ freeVars patt

instance Ord l => Free (Pattern l) l where
  freeVars (PattLit _) = mempty
  freeVars (PattBinder n) = Set.singleton n
  freeVars (PattWildcard _) = mempty
  freeVars (PattConstructor _ ps) = foldMap freeVars ps

instance Ord l => Free (Declaration l) l where
  freeVars (ConstDecl ns t expr _) =
    Set.fromList ns <> freeVars t <> freeVars expr
  freeVars (VarDecl ns t expr _) =
    Set.fromList ns <> freeVars t <> freeVars expr

instance Ord l => Free (Stmt l) l where
  freeVars (Skip _) = mempty
  freeVars (Abort _) = mempty
  freeVars (Assign ns es _) =
    Set.fromList ns <> Set.unions (map freeVars es)
  freeVars (AAssign a i e _) =
    freeVars a <> freeVars i <> freeVars e
  freeVars (Assert p _) = freeVars p
  freeVars (LoopInvariant p b _) = freeVars p <> freeVars b
  freeVars (Do gdcmds _) = Set.unions (map freeVars gdcmds)
  freeVars (If gdcmds _) = Set.unions (map freeVars gdcmds)
  freeVars (Spec _ _) = mempty
  freeVars Proof {} = mempty
  freeVars (Alloc x es _) =
    Set.singleton x <> Set.unions (map freeVars es)
  freeVars (HLookup x e _) =
    Set.singleton x <> freeVars e
  freeVars (HMutate e1 e2 _) = freeVars e1 <> freeVars e2
  freeVars (Dispose e _) = freeVars e
  freeVars (Block prog _) = freeVars prog

instance Ord l => Free (GdCmd l) l where
  freeVars (GdCmd g stmts _) =
    freeVars g <> Set.unions (map freeVars stmts)

instance Ord l => Free (Program l) l where
  freeVars (Program _defns decls props stmts _) =
    foldMap freeVars decls <> foldMap freeVars props <> foldMap freeVars stmts

-- SCM: TODO: deal with defns later.

toStateT :: (Monad m) => r -> RWST r w s m a -> StateT s m a
toStateT r m =
  StateT
    ( \s -> do
        (a, s', _) <- runRWST m r s
        return (a, s')
    )

toEvalStateT :: (Monad m) => r -> RWST r w s m a -> StateT s m (a, w)
toEvalStateT r m =
  StateT
    ( \s -> do
        (a, s', w) <- runRWST m r s
        return ((a, w), s')
    )
