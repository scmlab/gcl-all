module Syntax.Typed.Reduce where

import Control.Arrow ((***))
import Control.Monad (foldM)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GCL.Common (Free (..), Fresh (..))
import Syntax.Abstract.Types (Pattern (..), extractBinder)
import Syntax.Common.Types (Name (..), nameToText)
import Syntax.Substitution
import Syntax.Typed.Instances.Free ()
import Syntax.Typed.Instances.Substitution ()
import Syntax.Typed.Reduce.Saturation (saturatedRedex)
import Syntax.Typed.Types

type Redex = [Int] -- path to a redex

redexes :: Expr -> [Redex]
redexes (Lit _ _ _) = []
redexes (Var _ _ _) = []
redexes (Const _ _ _) = []
redexes (Op _ _) = []
redexes (Chain _) = [] -- should look into Chain. Omit for now.
redexes (App f@(Lam _ _ _ _) e _) =
  []
    : map (0 :) (redexes f)
    ++ map (1 :) (redexes e)
redexes (App (Var _ _ _) e _) = [] : map (1 :) (redexes e)
redexes (App f e _) = map (0 :) (redexes f) ++ map (1 :) (redexes e)
redexes (Lam _ _ e _) = map (0 :) (redexes e)
redexes (Tuple es) = redexesExprs 0 es
redexes (OutT _ t@(Tuple _)) = [] : map (0 :) (redexes t)
redexes (OutT _ _) = []
redexes (Quant _ _ r b _) = map (0 :) (redexes r) ++ map (1 :) (redexes b)
redexes (ArrIdx a i _) = map (0 :) (redexes a) ++ map (1 :) (redexes i)
redexes (ArrUpd a i e _) =
  map (0 :) (redexes a)
    ++ map (1 :) (redexes i)
    ++ map (2 :) (redexes e)
redexes (Case e cls _) = [] : map (0 :) (redexes e) ++ redexesExprs 1 (map getClause cls)
  where
    getClause (CaseClause _ e) = e
redexes (Subst e sb) = [] : map (0 :) (redexes e) ++ redexesExprs 1 (map snd sb)
redexes (EHole {}) = []

redexesExprs :: Int -> [Expr] -> [Redex]
redexesExprs i es =
  concat (zipWith (\i -> map (i :)) [i ..] (map redexes es))

-- a redex tree marks whether a node is a redex

data RTree a = Node a [RTree a] -- a rose tree
  deriving (Show)

type RT = RTree Bool

leaf :: RT
leaf = Node False []

redexRT :: Env -> Expr -> RT
redexRT _env (Lit _ _ _) = leaf
redexRT _env (Var _ _ _) = leaf
redexRT _env (Const _ _ _) = leaf
redexRT _env (Op _ _) = leaf
redexRT env (Chain ch) = Node False (redexRTChain env ch)
redexRT env (App f@(Lam _ _ _ _) e _) = Node True [redexRT env f, redexRT env e]
redexRT env (App v@(Var f _ _) e _) = Node (isDefined env f) [redexRT env v, redexRT env e]
redexRT env (App f e _) = Node False [redexRT env f, redexRT env e]
redexRT env (Lam x _ e _) = Node False [redexRT (shadowDefinitions [x] env) e]
redexRT env (Tuple es) = Node False (map (redexRT env) es)
redexRT env (OutT _ t@(Tuple _)) = Node True [redexRT env t]
redexRT env (OutT _ e) = Node False [redexRT env e]
redexRT env (Quant _ xs r b _) = Node False [redexRT env' r, redexRT env' b]
  where
    env' = shadowDefinitions (map fst xs) env
redexRT env (ArrIdx a i _) = Node False [redexRT env a, redexRT env i]
redexRT env (ArrUpd a i e _) =
  Node False [redexRT env a, redexRT env i, redexRT env e]
redexRT env (Case e cls _) =
  Node True (redexRT env e : map redexClause cls)
  where
    redexClause (CaseClause pattern' rhs) = redexRT (shadowDefinitions (extractBinder pattern') env) rhs
redexRT env (Subst e sb) =
  Node True (redexRT env e : map (redexRT env . snd) sb)
redexRT _env (EHole {}) = leaf

redexRTChain :: Env -> Chain -> [RT]
redexRTChain env (Pure e) = [redexRT env e]
redexRTChain env (More ch _ _ e) = redexRT env e : redexRTChain env ch

-- Saturated-redex marking (redexRT_sat) --------------------------------------
--
-- Like redexRT, but an application spine is marked as a redex ONLY when it is
-- "saturated": its head is a reducible function (a defined Var or a Lam) that
-- has been applied to enough arguments that its own result is no longer a
-- function. Partial applications stay unmarked (kept symbolic). Every other
-- constructor mirrors redexRT exactly, so the tree shape -- and hence the
-- render zipper paths -- is identical.

redexRT_sat :: Env -> Expr -> RT
redexRT_sat _env (Lit _ _ _) = leaf
redexRT_sat _env (Var _ _ _) = leaf
redexRT_sat _env (Const _ _ _) = leaf
redexRT_sat _env (Op _ _) = leaf
redexRT_sat env (Chain ch) = Node False (redexRTChain_sat env ch)
redexRT_sat env e@(App f a _) = Node (saturatedDefinitionRedex env e) [redexRT_sat env f, redexRT_sat env a]
redexRT_sat env (Lam x _ e _) = Node False [redexRT_sat (shadowDefinitions [x] env) e]
redexRT_sat env (Tuple es) = Node False (map (redexRT_sat env) es)
redexRT_sat env (OutT _ t@(Tuple _)) = Node True [redexRT_sat env t]
redexRT_sat env (OutT _ e) = Node False [redexRT_sat env e]
redexRT_sat env (Quant _ xs r b _) = Node False [redexRT_sat env' r, redexRT_sat env' b]
  where
    env' = shadowDefinitions (map fst xs) env
redexRT_sat env (ArrIdx a i _) = Node False [redexRT_sat env a, redexRT_sat env i]
redexRT_sat env (ArrUpd a i e _) =
  Node False [redexRT_sat env a, redexRT_sat env i, redexRT_sat env e]
redexRT_sat env (Case e cls _) =
  Node True (redexRT_sat env e : map redexClause cls)
  where
    redexClause (CaseClause pattern' rhs) = redexRT_sat (shadowDefinitions (extractBinder pattern') env) rhs
redexRT_sat env (Subst e sb) =
  Node True (redexRT_sat env e : map (redexRT_sat env . snd) sb)
redexRT_sat _env (EHole {}) = leaf

redexRTChain_sat :: Env -> Chain -> [RT]
redexRTChain_sat env (Pure e) = [redexRT_sat env e]
redexRTChain_sat env (More ch _ _ e) = redexRT_sat env e : redexRTChain_sat env ch

-- counting from the rightmost expression
-- simply because it makes things easier.

type RZ = ([Int], RT) --- a "zipper" for RT

initRZ :: [Int] -> RT -> RZ
initRZ prefix rt = (reverse prefix, rt)

isRedex :: RZ -> Bool
isRedex (_, Node b _) = b

currentPath :: RZ -> [Int]
currentPath (p, _) = reverse p

descend :: RZ -> [RZ]
descend (p, Node _ ts) = zipWith (\i t -> (i : p, t)) [0 ..] ts

-- reduce

type Env = [(Name, Expr)]

-- A lexical binder with the same name as a global definition hides that
-- definition while traversing the binder's scope.
shadowDefinitions :: [Name] -> Env -> Env
shadowDefinitions names =
  filter (\(name, _) -> name `notElem` names)

isDefined :: Env -> Name -> Bool
isDefined env name =
  case lookup name env of
    Just _ -> True
    Nothing -> False

saturatedDefinitionRedex :: Env -> Expr -> Bool
saturatedDefinitionRedex env expr =
  saturatedRedex expr
    && case applicationHead expr of
      Var name _ _ -> isDefined env name
      Lam {} -> True
      _ -> False

applicationHead :: Expr -> Expr
applicationHead (App function _ _) = applicationHead function
applicationHead e = e

-- Find the expression selected by a render redex path while carrying the same
-- lexically-shadowed definition environment used by redex marking/reduction.
focusAt :: Env -> Expr -> Redex -> Maybe (Env, Expr)
focusAt env expr [] = Just (env, expr)
focusAt env (Chain chain) (i : path) = focusAtChain env chain i path
focusAt env (App function _ _) (0 : path) = focusAt env function path
focusAt env (App _ argument _) (1 : path) = focusAt env argument path
focusAt env (Lam binder _ body _) (0 : path) =
  focusAt (shadowDefinitions [binder] env) body path
focusAt env (Tuple exprs) (i : path) =
  atMay exprs i >>= \expr -> focusAt env expr path
focusAt env (OutT _ expr) (0 : path) = focusAt env expr path
focusAt env (Quant _ binders range _ _) (0 : path) =
  focusAt env' range path
  where
    env' = shadowDefinitions (map fst binders) env
focusAt env (Quant _ binders _ body _) (1 : path) =
  focusAt env' body path
  where
    env' = shadowDefinitions (map fst binders) env
focusAt env (ArrIdx array _ _) (0 : path) = focusAt env array path
focusAt env (ArrIdx _ index _) (1 : path) = focusAt env index path
focusAt env (ArrUpd array _ _ _) (0 : path) = focusAt env array path
focusAt env (ArrUpd _ index _ _) (1 : path) = focusAt env index path
focusAt env (ArrUpd _ _ value _) (2 : path) = focusAt env value path
focusAt env (Case scrutinee _ _) (0 : path) = focusAt env scrutinee path
focusAt env (Case _ clauses _) (i : path)
  | i > 0 = do
      CaseClause pattern' rhs <- atMay clauses (i - 1)
      focusAt (shadowDefinitions (extractBinder pattern') env) rhs path
focusAt env (Subst subject _) (0 : path) = focusAt env subject path
focusAt env (Subst _ substitutions) (i : path)
  | i > 0 = snd <$> atMay substitutions (i - 1) >>= \expr -> focusAt env expr path
focusAt _ _ _ = Nothing

focusAtChain :: Env -> Chain -> Int -> Redex -> Maybe (Env, Expr)
focusAtChain env (Pure expr) 0 path = focusAt env expr path
focusAtChain env (More _ _ _ expr) 0 path = focusAt env expr path
focusAtChain env (More chain _ _ _) i path
  | i > 0 = focusAtChain env chain (i - 1) path
focusAtChain _ _ _ _ = Nothing

atMay :: [a] -> Int -> Maybe a
atMay _ i | i < 0 = Nothing
atMay [] _ = Nothing
atMay (x : _) 0 = Just x
atMay (_ : xs) i = atMay xs (i - 1)

definitionAtPath :: Env -> Expr -> Redex -> Maybe Name
definitionAtPath env expr path = do
  (envAtFocus, focus) <- focusAt env expr path
  case focus of
    App {} -> case applicationHead focus of
      Var name _ _ | isDefined envAtFocus name -> Just name
      _ -> Nothing
    _ -> Nothing

-- Include the free names of referenced definitions as well as the immediate
-- RHS. This is conservative, but protects point-free chains that the current
-- root reducer can unfold during a single click.
definitionClosureFreeVars :: Env -> Name -> Set Text
definitionClosureFreeVars env = go Set.empty
  where
    go visited name
      | nameText `Set.member` visited = Set.empty
      | otherwise = case lookup name env of
          Nothing -> Set.empty
          Just rhs ->
            let direct = freeVarsT rhs
                visited' = Set.insert nameText visited
                dependencies =
                  [ dependency
                  | (dependency, _) <- env,
                    nameToText dependency `Set.member` direct
                  ]
             in direct <> foldMap (go visited') dependencies
      where
        nameText = nameToText name

type NameRenaming = Map.Map Text Text

alphaRenameAlongPath ::
  (Fresh m) =>
  Env ->
  Set Text ->
  Expr ->
  Redex ->
  m Expr
alphaRenameAlongPath env avoid expr path =
  fst <$> alphaExpr avoid reserved expr path
  where
    reserved = avoid <> allNamesExpr expr <> allNamesEnv env

alphaExpr ::
  (Fresh m) =>
  Set Text ->
  Set Text ->
  Expr ->
  Redex ->
  m (Expr, Set Text)
alphaExpr _ reserved expr [] = return (expr, reserved)
alphaExpr avoid reserved (Chain chain) (i : path) = do
  (chain', reserved') <- alphaChain avoid reserved chain i path
  return (Chain chain', reserved')
alphaExpr avoid reserved (App function argument range) (0 : path) = do
  (function', reserved') <- alphaExpr avoid reserved function path
  return (App function' argument range, reserved')
alphaExpr avoid reserved (App function argument range) (1 : path) = do
  (argument', reserved') <- alphaExpr avoid reserved argument path
  return (App function argument' range, reserved')
alphaExpr avoid reserved (Lam binder ty body range) (0 : path) = do
  (renaming, reserved') <- freshenBinders avoid reserved [binder]
  let binder' = renameName renaming binder
      body' = renameFreeOccurrences renaming body
  (body'', reserved'') <- alphaExpr avoid reserved' body' path
  return (Lam binder' ty body'' range, reserved'')
alphaExpr avoid reserved (Tuple exprs) (i : path) = do
  (exprs', reserved') <- alphaNthExpr avoid reserved i exprs path
  return (Tuple exprs', reserved')
alphaExpr avoid reserved (OutT index expr) (0 : path) = do
  (expr', reserved') <- alphaExpr avoid reserved expr path
  return (OutT index expr', reserved')
alphaExpr avoid reserved (Quant op binders range body location) (i : path)
  | i == 0 || i == 1 = do
      (renaming, reserved') <- freshenBinders avoid reserved (map fst binders)
      let binders' = map (\(name, ty) -> (renameName renaming name, ty)) binders
          op' = renameFreeOccurrences renaming op
          range' = renameFreeOccurrences renaming range
          body' = renameFreeOccurrences renaming body
      if i == 0
        then do
          (range'', reserved'') <- alphaExpr avoid reserved' range' path
          return (Quant op' binders' range'' body' location, reserved'')
        else do
          (body'', reserved'') <- alphaExpr avoid reserved' body' path
          return (Quant op' binders' range' body'' location, reserved'')
alphaExpr avoid reserved (ArrIdx array index location) (0 : path) = do
  (array', reserved') <- alphaExpr avoid reserved array path
  return (ArrIdx array' index location, reserved')
alphaExpr avoid reserved (ArrIdx array index location) (1 : path) = do
  (index', reserved') <- alphaExpr avoid reserved index path
  return (ArrIdx array index' location, reserved')
alphaExpr avoid reserved (ArrUpd array index value location) (0 : path) = do
  (array', reserved') <- alphaExpr avoid reserved array path
  return (ArrUpd array' index value location, reserved')
alphaExpr avoid reserved (ArrUpd array index value location) (1 : path) = do
  (index', reserved') <- alphaExpr avoid reserved index path
  return (ArrUpd array index' value location, reserved')
alphaExpr avoid reserved (ArrUpd array index value location) (2 : path) = do
  (value', reserved') <- alphaExpr avoid reserved value path
  return (ArrUpd array index value' location, reserved')
alphaExpr avoid reserved (Case scrutinee clauses location) (0 : path) = do
  (scrutinee', reserved') <- alphaExpr avoid reserved scrutinee path
  return (Case scrutinee' clauses location, reserved')
alphaExpr avoid reserved (Case scrutinee clauses location) (i : path)
  | i > 0 = do
      (clauses', reserved') <- alphaNthClause avoid reserved (i - 1) clauses path
      return (Case scrutinee clauses' location, reserved')
alphaExpr avoid reserved (Subst subject substitutions) (0 : path) = do
  (subject', reserved') <- alphaExpr avoid reserved subject path
  return (Subst subject' substitutions, reserved')
alphaExpr avoid reserved (Subst subject substitutions) (i : path)
  | i > 0 = do
      (substitutions', reserved') <-
        alphaNthSubstitution avoid reserved (i - 1) substitutions path
      return (Subst subject substitutions', reserved')
alphaExpr _ _ _ _ = error "definition path became invalid during alpha-renaming"

alphaChain ::
  (Fresh m) =>
  Set Text ->
  Set Text ->
  Chain ->
  Int ->
  Redex ->
  m (Chain, Set Text)
alphaChain avoid reserved (Pure expr) 0 path = do
  (expr', reserved') <- alphaExpr avoid reserved expr path
  return (Pure expr', reserved')
alphaChain avoid reserved (More chain op ty expr) 0 path = do
  (expr', reserved') <- alphaExpr avoid reserved expr path
  return (More chain op ty expr', reserved')
alphaChain avoid reserved (More chain op ty expr) i path
  | i > 0 = do
      (chain', reserved') <- alphaChain avoid reserved chain (i - 1) path
      return (More chain' op ty expr, reserved')
alphaChain _ _ _ _ _ = error "definition chain path became invalid during alpha-renaming"

alphaNthExpr ::
  (Fresh m) =>
  Set Text ->
  Set Text ->
  Int ->
  [Expr] ->
  Redex ->
  m ([Expr], Set Text)
alphaNthExpr _ _ _ [] _ = error "definition tuple path became invalid during alpha-renaming"
alphaNthExpr avoid reserved 0 (expr : exprs) path = do
  (expr', reserved') <- alphaExpr avoid reserved expr path
  return (expr' : exprs, reserved')
alphaNthExpr avoid reserved i (expr : exprs) path
  | i > 0 = do
      (exprs', reserved') <- alphaNthExpr avoid reserved (i - 1) exprs path
      return (expr : exprs', reserved')
alphaNthExpr _ _ _ _ _ = error "definition tuple path became invalid during alpha-renaming"

alphaNthClause ::
  (Fresh m) =>
  Set Text ->
  Set Text ->
  Int ->
  [CaseClause] ->
  Redex ->
  m ([CaseClause], Set Text)
alphaNthClause _ _ _ [] _ = error "definition case path became invalid during alpha-renaming"
alphaNthClause avoid reserved 0 (CaseClause pattern' rhs : clauses) path = do
  (renaming, reserved') <- freshenBinders avoid reserved (extractBinder pattern')
  let pattern'' = renamePatternBinders renaming pattern'
      rhs' = renameFreeOccurrences renaming rhs
  (rhs'', reserved'') <- alphaExpr avoid reserved' rhs' path
  return (CaseClause pattern'' rhs'' : clauses, reserved'')
alphaNthClause avoid reserved i (clause : clauses) path
  | i > 0 = do
      (clauses', reserved') <- alphaNthClause avoid reserved (i - 1) clauses path
      return (clause : clauses', reserved')
alphaNthClause _ _ _ _ _ = error "definition case path became invalid during alpha-renaming"

alphaNthSubstitution ::
  (Fresh m) =>
  Set Text ->
  Set Text ->
  Int ->
  [(Name, Expr)] ->
  Redex ->
  m ([(Name, Expr)], Set Text)
alphaNthSubstitution _ _ _ [] _ =
  error "definition substitution path became invalid during alpha-renaming"
alphaNthSubstitution avoid reserved 0 ((name, expr) : substitutions) path = do
  (expr', reserved') <- alphaExpr avoid reserved expr path
  return ((name, expr') : substitutions, reserved')
alphaNthSubstitution avoid reserved i (substitution : substitutions) path
  | i > 0 = do
      (substitutions', reserved') <-
        alphaNthSubstitution avoid reserved (i - 1) substitutions path
      return (substitution : substitutions', reserved')
alphaNthSubstitution _ _ _ _ _ =
  error "definition substitution path became invalid during alpha-renaming"

freshenBinders ::
  (Fresh m) =>
  Set Text ->
  Set Text ->
  [Name] ->
  m (NameRenaming, Set Text)
freshenBinders avoid reserved binders =
  foldM freshen (Map.empty, reserved) binders
  where
    freshen (renaming, usedNames) binder
      | oldText `Set.notMember` avoid = return (renaming, usedNames)
      | oldText `Map.member` renaming = return (renaming, usedNames)
      | otherwise = do
          newText <- freshAvoiding usedNames oldText
          return
            ( Map.insert oldText newText renaming,
              Set.insert newText usedNames
            )
      where
        oldText = nameToText binder

freshAvoiding :: (Fresh m) => Set Text -> Text -> m Text
freshAvoiding reserved prefix = do
  candidate <- freshPre prefix
  if candidate `Set.member` reserved
    then freshAvoiding reserved prefix
    else return candidate

renameFreeOccurrences :: NameRenaming -> Expr -> Expr
renameFreeOccurrences renaming expr = case expr of
  Lit {} -> expr
  Var name ty range -> Var (renameName renaming name) ty range
  Const {} -> expr
  Op {} -> expr
  Chain chain -> Chain (renameFreeOccurrencesChain renaming chain)
  App function argument range ->
    App
      (renameFreeOccurrences renaming function)
      (renameFreeOccurrences renaming argument)
      range
  Lam binder ty body range ->
    Lam binder ty (renameFreeOccurrences renaming' body) range
    where
      renaming' = deleteNames [binder] renaming
  Tuple exprs -> Tuple (map (renameFreeOccurrences renaming) exprs)
  OutT index inner -> OutT index (renameFreeOccurrences renaming inner)
  Quant op binders range body location ->
    Quant
      (renameFreeOccurrences renaming' op)
      binders
      (renameFreeOccurrences renaming' range)
      (renameFreeOccurrences renaming' body)
      location
    where
      renaming' = deleteNames (map fst binders) renaming
  ArrIdx array index location ->
    ArrIdx
      (renameFreeOccurrences renaming array)
      (renameFreeOccurrences renaming index)
      location
  ArrUpd array index value location ->
    ArrUpd
      (renameFreeOccurrences renaming array)
      (renameFreeOccurrences renaming index)
      (renameFreeOccurrences renaming value)
      location
  Case scrutinee clauses location ->
    Case
      (renameFreeOccurrences renaming scrutinee)
      (map renameClause clauses)
      location
    where
      renameClause (CaseClause pattern' rhs) =
        CaseClause
          pattern'
          (renameFreeOccurrences (deleteNames (extractBinder pattern') renaming) rhs)
  Subst subject substitutions ->
    Subst
      (renameFreeOccurrences subjectRenaming subject)
      (map (fmap (renameFreeOccurrences renaming)) substitutions)
    where
      subjectRenaming = deleteNames (map fst substitutions) renaming
  EHole {} -> expr

renameFreeOccurrencesChain :: NameRenaming -> Chain -> Chain
renameFreeOccurrencesChain renaming (Pure expr) =
  Pure (renameFreeOccurrences renaming expr)
renameFreeOccurrencesChain renaming (More chain op ty expr) =
  More
    (renameFreeOccurrencesChain renaming chain)
    op
    ty
    (renameFreeOccurrences renaming expr)

renamePatternBinders :: NameRenaming -> Pattern -> Pattern
renamePatternBinders _ pattern'@(PattLit _) = pattern'
renamePatternBinders renaming (PattBinder name) =
  PattBinder (renameName renaming name)
renamePatternBinders _ pattern'@(PattWildcard _) = pattern'
renamePatternBinders renaming (PattTuple patterns) =
  PattTuple (map (renamePatternBinders renaming) patterns)
renamePatternBinders renaming (PattConstructor name patterns) =
  PattConstructor name (map (renamePatternBinders renaming) patterns)

renameName :: NameRenaming -> Name -> Name
renameName renaming name@(Name oldText range) =
  case Map.lookup oldText renaming of
    Nothing -> name
    Just newText -> Name newText range

deleteNames :: [Name] -> NameRenaming -> NameRenaming
deleteNames names renaming =
  foldr (Map.delete . nameToText) renaming names

allNamesEnv :: Env -> Set Text
allNamesEnv env =
  Set.fromList (map (nameToText . fst) env)
    <> foldMap (allNamesExpr . snd) env

allNamesExpr :: Expr -> Set Text
allNamesExpr expr = case expr of
  Lit {} -> Set.empty
  Var name _ _ -> Set.singleton (nameToText name)
  Const name _ _ -> Set.singleton (nameToText name)
  Op {} -> Set.empty
  Chain chain -> allNamesChain chain
  App function argument _ -> allNamesExpr function <> allNamesExpr argument
  Lam binder _ body _ -> Set.insert (nameToText binder) (allNamesExpr body)
  Tuple exprs -> foldMap allNamesExpr exprs
  OutT _ inner -> allNamesExpr inner
  Quant op binders range body _ ->
    Set.fromList (map (nameToText . fst) binders)
      <> allNamesExpr op
      <> allNamesExpr range
      <> allNamesExpr body
  ArrIdx array index _ -> allNamesExpr array <> allNamesExpr index
  ArrUpd array index value _ ->
    allNamesExpr array <> allNamesExpr index <> allNamesExpr value
  Case scrutinee clauses _ ->
    allNamesExpr scrutinee <> foldMap allNamesClause clauses
  Subst subject substitutions ->
    allNamesExpr subject
      <> Set.fromList (map (nameToText . fst) substitutions)
      <> foldMap (allNamesExpr . snd) substitutions
  EHole (Hole _ _ _ _ holeEnv) ->
    Set.fromList (map nameToText (Map.keys holeEnv))

allNamesChain :: Chain -> Set Text
allNamesChain (Pure expr) = allNamesExpr expr
allNamesChain (More chain _ _ expr) = allNamesChain chain <> allNamesExpr expr

allNamesClause :: CaseClause -> Set Text
allNamesClause (CaseClause pattern' rhs) =
  allNamesPattern pattern' <> allNamesExpr rhs

allNamesPattern :: Pattern -> Set Text
allNamesPattern (PattLit _) = Set.empty
allNamesPattern (PattBinder name) = Set.singleton (nameToText name)
allNamesPattern (PattWildcard _) = Set.empty
allNamesPattern (PattTuple patterns) = foldMap allNamesPattern patterns
allNamesPattern (PattConstructor name patterns) =
  Set.insert (nameToText name) (foldMap allNamesPattern patterns)

reduce :: (Fresh m) => Env -> Expr -> Redex -> m Expr
reduce env expr path =
  case definitionAtPath env expr path of
    Nothing -> reduceRaw env expr path
    Just definitionName -> do
      let avoid = definitionClosureFreeVars env definitionName
      expr' <- alphaRenameAlongPath env avoid expr path
      reduceRaw env expr' path

reduceRaw :: (Fresh m) => Env -> Expr -> Redex -> m Expr
reduceRaw env (Chain ch) (i : p) = Chain <$> reduceChain env ch i p
reduceRaw _env (App (Lam x _ bdy _) e _) [] = betaReduce x bdy e
reduceRaw env exp@(App (Var f _ _) e r) [] =
  maybe
    (return exp)
    (\rhs -> reduceRaw env (App rhs e r) [])
    (lookup f env)
-- The function position is neither a lambda nor a variable. This happens when
-- a point-free definition is inlined into an application, e.g. `id2 = plus 0`
-- or `id2 = case c of ... -> id`, leaving a partial application / `case` /
-- substitution in the function position. Reduce the function position one step
-- so it can progress towards a lambda; if it cannot make progress, leave the
-- application untouched instead of falling through to the catch-all error.
reduceRaw env exp@(App f e r) []
  | rootReducible f = do
      f' <- reduceRaw env f []
      if f' == f then return exp else reduceRaw env (App f' e r) []
  | otherwise = return exp
reduceRaw env (App f e r) (0 : p) = App <$> reduceRaw env f p <*> pure e <*> pure r
reduceRaw env (App f e r) (1 : p) = App f <$> reduceRaw env e p <*> pure r
reduceRaw env (Lam x t e r) (0 : p) = Lam x t <$> reduceRaw (shadowDefinitions [x] env) e p <*> pure r
reduceRaw env (Tuple es) (n : p) = Tuple <$> reduceNth env n es p
reduceRaw _env (OutT i (Tuple es)) [] = return (es !! i)
reduceRaw env (OutT i e) (0 : p) = OutT i <$> reduceRaw env e p
reduceRaw env (Quant op xs ran bdy r) (0 : p) =
  Quant op xs <$> reduceRaw env' ran p <*> pure bdy <*> pure r
  where
    env' = shadowDefinitions (map fst xs) env
reduceRaw env (Quant op xs ran bdy r) (1 : p) =
  Quant op xs ran <$> reduceRaw env' bdy p <*> pure r
  where
    env' = shadowDefinitions (map fst xs) env
reduceRaw env (ArrIdx a i r) (0 : p) = ArrIdx <$> reduceRaw env a p <*> pure i <*> pure r
reduceRaw env (ArrIdx a i r) (1 : p) = ArrIdx a <$> reduceRaw env i p <*> pure r
reduceRaw env (ArrUpd a i e r) (0 : p) =
  ArrUpd <$> reduceRaw env a p <*> pure i <*> pure e <*> pure r
reduceRaw env (ArrUpd a i e r) (1 : p) = ArrUpd a <$> reduceRaw env i p <*> pure e <*> pure r
reduceRaw env (ArrUpd a i e r) (2 : p) = ArrUpd a i <$> reduceRaw env e p <*> pure r
reduceRaw env expr@(Case e cls _) [] =
  maybe expr id <$> reduceCase env e cls -- return expr unchanged if cannot reduce
reduceRaw env (Case e cls r) (0 : p) =
  Case <$> reduceRaw env e p <*> pure cls <*> pure r
reduceRaw env (Case e cls r) (n : p) =
  Case e <$> reduceNthCaseClause env (n - 1) cls p <*> pure r
reduceRaw _env (Subst e sb) [] = subst (map (nameToText *** id) sb) e
reduceRaw env (Subst e sb) (0 : p) = Subst <$> reduceRaw env e p <*> pure sb
reduceRaw env (Subst e sb) (n : p) =
  (Subst e . zip (map fst sb))
    <$> reduceNth env (n - 1) (map snd sb) p
reduceRaw _ _ _ = error "shouldn't happen" -- a "catch-all" clause

reduceNth :: (Fresh m) => Env -> Int -> [Expr] -> Redex -> m [Expr]
reduceNth _ _ [] _ = error "shouldn't happen"
reduceNth env 0 (e : es) p = (: es) <$> reduceRaw env e p
reduceNth env n (e : es) p = (e :) <$> reduceNth env (n - 1) es p

reduceNthCaseClause :: (Fresh m) => Env -> Int -> [CaseClause] -> Redex -> m [CaseClause]
reduceNthCaseClause _ _ [] _ = error "shouldn't happen"
reduceNthCaseClause env 0 (CaseClause pattern' rhs : clauses) path =
  (\rhs' -> CaseClause pattern' rhs' : clauses)
    <$> reduceRaw (shadowDefinitions (extractBinder pattern') env) rhs path
reduceNthCaseClause env n (clause : clauses) path =
  (clause :) <$> reduceNthCaseClause env (n - 1) clauses path

betaReduce :: (Fresh m) => Name -> Expr -> Expr -> m Expr
betaReduce x bdy e = subst [(nameToText x, e)] bdy

-- | Whether an expression is reducible at its very root, i.e. whether
--   @reduceRaw env e []@ performs a genuine one-step reduction. Used to decide
--   whether the function position of an application can be simplified before
--   applying.
rootReducible :: Expr -> Bool
rootReducible (App (Lam {}) _ _) = True
rootReducible (App (Var {}) _ _) = True
rootReducible (Case {}) = True
rootReducible (Subst {}) = True
rootReducible (OutT _ (Tuple {})) = True
rootReducible _ = False

reduceChain :: (Fresh m) => Env -> Chain -> Int -> Redex -> m Chain
reduceChain env (Pure e) 0 p = Pure <$> reduceRaw env e p
reduceChain env (More ch op t e) 0 p = More ch op t <$> reduceRaw env e p
reduceChain env (More ch op t e) i p =
  (\ch' -> More ch' op t e) <$> reduceChain env ch (i - 1) p
reduceChain _ _ _ _ = error "shouldn't happen (reduceChain)"

reduceCase :: (Fresh m) => Env -> Expr -> [CaseClause] -> m (Maybe Expr)
reduceCase _env _e [] = return Nothing
reduceCase env e (CaseClause ptn rhs : cls) = do
  case matchPattern e ptn of
    Just subs -> Just <$> subst subs rhs
    Nothing -> reduceCase env e cls

matchPattern :: Expr -> Pattern -> Maybe (Subst Expr)
matchPattern (Lit l _ _) (PattLit l') | l == l' = Just []
matchPattern e (PattBinder v) = Just [(nameToText v, e)]
matchPattern _ (PattWildcard _) = Just []
matchPattern (Tuple es) (PattTuple ps)
  | length es == length ps =
      concat <$> joinMaybe (zipWith matchPattern es ps)
-- matchPattern e (PattConstructor c ps)
-- todo: SCM: to be implemented!
matchPattern _ _ = Nothing

joinMaybe :: [Maybe a] -> Maybe [a]
joinMaybe [] = Just []
joinMaybe (Nothing : _) = Nothing
joinMaybe (Just x : xs) = (x :) <$> joinMaybe xs
