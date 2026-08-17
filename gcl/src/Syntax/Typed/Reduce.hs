module Syntax.Typed.Reduce where

import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GCL.Common (Free (..), Fresh (..))
import Syntax.Abstract.Types (Pattern (..), extractBinder)
import Syntax.Common.Types (Name (..), nameToText)
import Syntax.Typed.Instances.Free ()
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
redexRT env (App f@(Lam binder _ body _) e _) =
  Node
    (not (betaDeferred env binder body e))
    [redexRT env f, redexRT env e]
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
  Node (not (caseDeferred env e cls)) (redexRT env e : map redexClause cls)
  where
    redexClause (CaseClause pattern' rhs) = redexRT (shadowDefinitions (extractBinder pattern') env) rhs
redexRT env expression@(Subst e sb) =
  Node (substitutionMarkedRedex env expression) (redexRT env e : map (redexRT env . snd) sb)
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
redexRT_sat env e@(App f a _) =
  Node (saturatedDefinitionRedex env e) [redexRT_sat env f, redexRT_sat env a]
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
  Node (not (caseDeferred env e cls)) (redexRT_sat env e : map redexClause cls)
  where
    redexClause (CaseClause pattern' rhs) = redexRT_sat (shadowDefinitions (extractBinder pattern') env) rhs
redexRT_sat env expression@(Subst e sb) =
  Node (substitutionMarkedRedex env expression) (redexRT_sat env e : map (redexRT_sat env . snd) sb)
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
      Lam {} -> not (applicationSpineDeferred env expr)
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
    Subst (Var name _ _) _
      | isDefined envAtFocus name -> Just name
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

alphaRenameAlongPath :: Env -> Set Text -> Expr -> Redex -> Expr
alphaRenameAlongPath env avoid expr path =
  fst (alphaExpr avoid reserved expr path)
  where
    reserved = avoid <> allNamesExpr expr <> allNamesEnv env

alphaExpr ::
  Set Text ->
  Set Text ->
  Expr ->
  Redex ->
  (Expr, Set Text)
alphaExpr _ reserved expr [] = (expr, reserved)
alphaExpr avoid reserved (Chain chain) (i : path) =
  let (chain', reserved') = alphaChain avoid reserved chain i path
   in (Chain chain', reserved')
alphaExpr avoid reserved (App function argument range) (0 : path) =
  let (function', reserved') = alphaExpr avoid reserved function path
   in (App function' argument range, reserved')
alphaExpr avoid reserved (App function argument range) (1 : path) =
  let (argument', reserved') = alphaExpr avoid reserved argument path
   in (App function argument' range, reserved')
alphaExpr avoid reserved (Lam binder ty body range) (0 : path) =
  let (renaming, reserved') = freshenBinders avoid reserved [binder]
      binder' = renameName renaming binder
      body' = renameFreeOccurrences renaming body
      (body'', reserved'') = alphaExpr avoid reserved' body' path
   in (Lam binder' ty body'' range, reserved'')
alphaExpr avoid reserved (Tuple exprs) (i : path) =
  let (exprs', reserved') = alphaNthExpr avoid reserved i exprs path
   in (Tuple exprs', reserved')
alphaExpr avoid reserved (OutT index expr) (0 : path) =
  let (expr', reserved') = alphaExpr avoid reserved expr path
   in (OutT index expr', reserved')
alphaExpr avoid reserved (Quant op binders range body location) (i : path)
  | i == 0 || i == 1 =
      let (renaming, reserved') = freshenBinders avoid reserved (map fst binders)
          binders' = map (\(name, ty) -> (renameName renaming name, ty)) binders
          op' = renameFreeOccurrences renaming op
          range' = renameFreeOccurrences renaming range
          body' = renameFreeOccurrences renaming body
       in if i == 0
            then
              let (range'', reserved'') = alphaExpr avoid reserved' range' path
               in (Quant op' binders' range'' body' location, reserved'')
            else
              let (body'', reserved'') = alphaExpr avoid reserved' body' path
               in (Quant op' binders' range' body'' location, reserved'')
alphaExpr avoid reserved (ArrIdx array index location) (0 : path) =
  let (array', reserved') = alphaExpr avoid reserved array path
   in (ArrIdx array' index location, reserved')
alphaExpr avoid reserved (ArrIdx array index location) (1 : path) =
  let (index', reserved') = alphaExpr avoid reserved index path
   in (ArrIdx array index' location, reserved')
alphaExpr avoid reserved (ArrUpd array index value location) (0 : path) =
  let (array', reserved') = alphaExpr avoid reserved array path
   in (ArrUpd array' index value location, reserved')
alphaExpr avoid reserved (ArrUpd array index value location) (1 : path) =
  let (index', reserved') = alphaExpr avoid reserved index path
   in (ArrUpd array index' value location, reserved')
alphaExpr avoid reserved (ArrUpd array index value location) (2 : path) =
  let (value', reserved') = alphaExpr avoid reserved value path
   in (ArrUpd array index value' location, reserved')
alphaExpr avoid reserved (Case scrutinee clauses location) (0 : path) =
  let (scrutinee', reserved') = alphaExpr avoid reserved scrutinee path
   in (Case scrutinee' clauses location, reserved')
alphaExpr avoid reserved (Case scrutinee clauses location) (i : path)
  | i > 0 =
      let (clauses', reserved') = alphaNthClause avoid reserved (i - 1) clauses path
       in (Case scrutinee clauses' location, reserved')
alphaExpr avoid reserved (Subst subject substitutions) (0 : path) =
  let (subject', reserved') = alphaExpr avoid reserved subject path
   in (Subst subject' substitutions, reserved')
alphaExpr avoid reserved (Subst subject substitutions) (i : path)
  | i > 0 =
      let (substitutions', reserved') =
            alphaNthSubstitution avoid reserved (i - 1) substitutions path
       in (Subst subject substitutions', reserved')
alphaExpr _ _ _ _ = error "definition path became invalid during alpha-renaming"

alphaChain ::
  Set Text ->
  Set Text ->
  Chain ->
  Int ->
  Redex ->
  (Chain, Set Text)
alphaChain avoid reserved (Pure expr) 0 path =
  let (expr', reserved') = alphaExpr avoid reserved expr path
   in (Pure expr', reserved')
alphaChain avoid reserved (More chain op ty expr) 0 path =
  let (expr', reserved') = alphaExpr avoid reserved expr path
   in (More chain op ty expr', reserved')
alphaChain avoid reserved (More chain op ty expr) i path
  | i > 0 =
      let (chain', reserved') = alphaChain avoid reserved chain (i - 1) path
       in (More chain' op ty expr, reserved')
alphaChain _ _ _ _ _ = error "definition chain path became invalid during alpha-renaming"

alphaNthExpr ::
  Set Text ->
  Set Text ->
  Int ->
  [Expr] ->
  Redex ->
  ([Expr], Set Text)
alphaNthExpr _ _ _ [] _ = error "definition tuple path became invalid during alpha-renaming"
alphaNthExpr avoid reserved 0 (expr : exprs) path =
  let (expr', reserved') = alphaExpr avoid reserved expr path
   in (expr' : exprs, reserved')
alphaNthExpr avoid reserved i (expr : exprs) path
  | i > 0 =
      let (exprs', reserved') = alphaNthExpr avoid reserved (i - 1) exprs path
       in (expr : exprs', reserved')
alphaNthExpr _ _ _ _ _ = error "definition tuple path became invalid during alpha-renaming"

alphaNthClause ::
  Set Text ->
  Set Text ->
  Int ->
  [CaseClause] ->
  Redex ->
  ([CaseClause], Set Text)
alphaNthClause _ _ _ [] _ = error "definition case path became invalid during alpha-renaming"
alphaNthClause avoid reserved 0 (CaseClause pattern' rhs : clauses) path =
  let (renaming, reserved') = freshenBinders avoid reserved (extractBinder pattern')
      pattern'' = renamePatternBinders renaming pattern'
      rhs' = renameFreeOccurrences renaming rhs
      (rhs'', reserved'') = alphaExpr avoid reserved' rhs' path
   in (CaseClause pattern'' rhs'' : clauses, reserved'')
alphaNthClause avoid reserved i (clause : clauses) path
  | i > 0 =
      let (clauses', reserved') = alphaNthClause avoid reserved (i - 1) clauses path
       in (clause : clauses', reserved')
alphaNthClause _ _ _ _ _ = error "definition case path became invalid during alpha-renaming"

alphaNthSubstitution ::
  Set Text ->
  Set Text ->
  Int ->
  [(Name, Expr)] ->
  Redex ->
  ([(Name, Expr)], Set Text)
alphaNthSubstitution _ _ _ [] _ =
  error "definition substitution path became invalid during alpha-renaming"
alphaNthSubstitution avoid reserved 0 ((name, expr) : substitutions) path =
  let (expr', reserved') = alphaExpr avoid reserved expr path
   in ((name, expr') : substitutions, reserved')
alphaNthSubstitution avoid reserved i (substitution : substitutions) path
  | i > 0 =
      let (substitutions', reserved') =
            alphaNthSubstitution avoid reserved (i - 1) substitutions path
       in (substitution : substitutions', reserved')
alphaNthSubstitution _ _ _ _ _ =
  error "definition substitution path became invalid during alpha-renaming"

freshenBinders ::
  Set Text ->
  Set Text ->
  [Name] ->
  (NameRenaming, Set Text)
freshenBinders avoid reserved binders =
  foldl freshen (Map.empty, reserved) binders
  where
    freshen (renaming, usedNames) binder
      | oldText `Set.notMember` avoid = (renaming, usedNames)
      | oldText `Map.member` renaming = (renaming, usedNames)
      | otherwise =
          ( Map.insert oldText newText renaming,
            Set.insert newText usedNames
          )
      where
        oldText = nameToText binder
        newText = freshAvoiding usedNames oldText

freshAvoiding :: Set Text -> Text -> Text
freshAvoiding reserved prefix = go 0
  where
    go :: Int -> Text
    go suffix =
      let candidate = prefix <> Text.pack ('_' : show suffix)
       in if candidate `Set.member` reserved
            then go (suffix + 1)
            else candidate

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

data DefinitionSubstPolicy
  = SuspendAtDefinitions
  | PreserveExistingPending
  deriving (Eq, Show)

type DefinitionSubst = [(Name, Expr)]

containsVisibleDefinitionReference :: Env -> Expr -> Bool
containsVisibleDefinitionReference env expr = case expr of
  Lit {} -> False
  Var name _ _ -> isDefined env name
  Const {} -> False
  Op {} -> False
  Chain chain -> containsVisibleDefinitionChain env chain
  App function argument _ ->
    containsVisibleDefinitionReference env function
      || containsVisibleDefinitionReference env argument
  Lam binder _ body _ ->
    containsVisibleDefinitionReference (shadowDefinitions [binder] env) body
  Tuple exprs -> any (containsVisibleDefinitionReference env) exprs
  OutT _ inner -> containsVisibleDefinitionReference env inner
  Quant op binders range body _ ->
    any (containsVisibleDefinitionReference env') [op, range, body]
    where
      env' = shadowDefinitions (map fst binders) env
  ArrIdx array index _ ->
    containsVisibleDefinitionReference env array
      || containsVisibleDefinitionReference env index
  ArrUpd array index value _ ->
    any (containsVisibleDefinitionReference env) [array, index, value]
  Case scrutinee clauses _ ->
    containsVisibleDefinitionReference env scrutinee
      || any containsClause clauses
    where
      containsClause (CaseClause pattern' rhs) =
        containsVisibleDefinitionReference
          (shadowDefinitions (extractBinder pattern') env)
          rhs
  Subst subject substitutions ->
    containsVisibleDefinitionReference env subject
      || any (containsVisibleDefinitionReference env . snd) substitutions
  EHole {} -> False

containsVisibleDefinitionChain :: Env -> Chain -> Bool
containsVisibleDefinitionChain env (Pure expr) =
  containsVisibleDefinitionReference env expr
containsVisibleDefinitionChain env (More chain _ _ expr) =
  containsVisibleDefinitionChain env chain
    || containsVisibleDefinitionReference env expr

isPendingDefinition :: Env -> Expr -> Bool
isPendingDefinition env (Subst subject _) = case subject of
  Var name _ _ -> isDefined env name
  nested@Subst {} -> isPendingDefinition env nested
  _ -> False
isPendingDefinition _ _ = False

tableDomains :: DefinitionSubst -> Set Text
tableDomains = Set.fromList . map (nameToText . fst)

tableRangeFreeVars :: DefinitionSubst -> Set Text
tableRangeFreeVars = foldMap (freeVarsT . snd)

removeTableDomains :: [Name] -> DefinitionSubst -> DefinitionSubst
removeTableDomains binders =
  filter ((`Set.notMember` boundNames) . nameToText . fst)
  where
    boundNames = Set.fromList (map nameToText binders)

restrictTableToScopes :: [Expr] -> DefinitionSubst -> DefinitionSubst
restrictTableToScopes scopes =
  filter ((`Set.member` scopeFreeVars) . nameToText . fst)
  where
    scopeFreeVars = foldMap freeVarsT scopes

tableForTransparentScope ::
  [Name] ->
  [Expr] ->
  DefinitionSubst ->
  DefinitionSubst
-- Shared by preflight and traversal so their retained tables stay identical.
tableForTransparentScope binders scopes =
  restrictTableToScopes scopes . removeTableDomains binders

relevantDefinitionTable :: Env -> Name -> DefinitionSubst -> DefinitionSubst
relevantDefinitionTable globalEnv definitionName =
  filter
    ( (`Set.member` definitionClosureFreeVars globalEnv definitionName)
        . nameToText
        . fst
    )

tableForLexicalScope ::
  DefinitionSubstPolicy ->
  Env ->
  [Name] ->
  [Expr] ->
  DefinitionSubst ->
  DefinitionSubst
tableForLexicalScope policy env binders scopes table
  | policy == SuspendAtDefinitions && opaque = table
  | otherwise = tableForTransparentScope binders scopes table
  where
    env' = shadowDefinitions binders env
    opaque = any (containsVisibleDefinitionReference env') scopes

tableForSubstitutionSubject ::
  DefinitionSubstPolicy ->
  Env ->
  DefinitionSubst ->
  Expr ->
  DefinitionSubst ->
  DefinitionSubst
-- Explicit substitutions may need entries for hidden definition-site names;
-- lexical beta/case substitutions never enter those bodies. All nested-Subst
-- paths share this helper so capture checks and traversal cannot drift apart.
tableForSubstitutionSubject policy env outerTable subject innerTable
  | policy == SuspendAtDefinitions
      && containsVisibleDefinitionReference env subject =
      unshadowedTable
  | otherwise = restrictTableToScopes [subject] unshadowedTable
  where
    unshadowedTable = removeTableDomains (map fst innerTable) outerTable

nestedOpacityConflict ::
  DefinitionSubstPolicy ->
  Env ->
  DefinitionSubst ->
  Expr ->
  DefinitionSubst ->
  Bool
nestedOpacityConflict policy env outerTable innerSubject innerTable =
  not (Set.null captureConflicts)
    && containsVisibleDefinitionReference env innerSubject
  where
    innerDomains = tableDomains innerTable
    subjectTable =
      tableForSubstitutionSubject
        policy
        env
        outerTable
        innerSubject
        innerTable
    captureConflicts = innerDomains `Set.intersection` tableRangeFreeVars subjectTable

mustDeferSubstitution ::
  DefinitionSubstPolicy ->
  Env ->
  DefinitionSubst ->
  Expr ->
  Bool
mustDeferSubstitution _ _ [] _ = False
mustDeferSubstitution policy env table expr = case expr of
  Lit {} -> False
  Var {} -> False
  Const {} -> False
  Op {} -> False
  Chain chain -> mustDeferChain policy env table chain
  App function argument _ ->
    any (mustDeferSubstitution policy env table) [function, argument]
  Lam binder _ body _ ->
    mustDeferSubstitution policy env' table' body
    where
      env' = shadowDefinitions [binder] env
      table' = tableForLexicalScope policy env [binder] [body] table
  Tuple exprs -> any (mustDeferSubstitution policy env table) exprs
  OutT _ inner -> mustDeferSubstitution policy env table inner
  Quant op binders range body _ ->
    any (mustDeferSubstitution policy env' table') [op, range, body]
    where
      names = map fst binders
      env' = shadowDefinitions names env
      table' = tableForLexicalScope policy env names [op, range, body] table
  ArrIdx array index _ ->
    any (mustDeferSubstitution policy env table) [array, index]
  ArrUpd array index value _ ->
    any (mustDeferSubstitution policy env table) [array, index, value]
  Case scrutinee clauses _ ->
    mustDeferSubstitution policy env table scrutinee
      || any deferClause clauses
    where
      deferClause (CaseClause pattern' rhs) =
        mustDeferSubstitution policy env' table' rhs
        where
          names = extractBinder pattern'
          env' = shadowDefinitions names env
          table' = tableForLexicalScope policy env names [rhs] table
  expression@(Subst subject innerTable)
    | isPendingDefinition env expression ->
        policy == PreserveExistingPending
          && ( mustDeferSubstitution policy env table subject
                 || any (mustDeferSubstitution policy env table . snd) innerTable
             )
    | nestedOpacityConflict policy env table subject innerTable -> True
    | otherwise ->
        mustDeferSubstitution policy env subjectTable subject
          || any (mustDeferSubstitution policy env table . snd) innerTable
    where
      subjectTable =
        tableForSubstitutionSubject policy env table subject innerTable
  EHole {} -> False

mustDeferChain ::
  DefinitionSubstPolicy ->
  Env ->
  DefinitionSubst ->
  Chain ->
  Bool
mustDeferChain policy env table (Pure expr) =
  mustDeferSubstitution policy env table expr
mustDeferChain policy env table (More chain _ _ expr) =
  mustDeferChain policy env table chain
    || mustDeferSubstitution policy env table expr

prepareLexicalBinders ::
  DefinitionSubstPolicy ->
  Env ->
  DefinitionSubst ->
  [Name] ->
  [Expr] ->
  Set Text ->
  (NameRenaming, [Name], DefinitionSubst, Env, Set Text)
prepareLexicalBinders policy env table binders scopes reserved =
  (renaming, binders', table', env', reserved')
  where
    scopeEnv = shadowDefinitions binders env
    opaque =
      policy == SuspendAtDefinitions
        && any (containsVisibleDefinitionReference scopeEnv) scopes
    table'
      | opaque = table
      | otherwise = tableForTransparentScope binders scopes table
    domainConflicts
      | opaque = tableDomains table
      | otherwise = Set.empty
    avoid = domainConflicts <> tableRangeFreeVars table'
    (renaming, reserved') = freshenBinders avoid reserved binders
    binders' = map (renameName renaming) binders
    env' = shadowDefinitions binders' env

substWithDefinitions ::
  DefinitionSubstPolicy ->
  Env ->
  Env ->
  DefinitionSubst ->
  Expr ->
  Maybe Expr
substWithDefinitions _ _ _ [] expr = Just expr
substWithDefinitions policy globalEnv env table expr =
  fst <$> substDefinitionExpr policy globalEnv env table expr reserved
  where
    reserved =
      allNamesExpr expr
        <> allNamesEnv globalEnv
        <> Set.fromList (map (nameToText . fst) table)
        <> foldMap (allNamesExpr . snd) table

substDefinitionExpr ::
  DefinitionSubstPolicy ->
  Env ->
  Env ->
  DefinitionSubst ->
  Expr ->
  Set Text ->
  Maybe (Expr, Set Text)
substDefinitionExpr _ _ _ [] expr reserved = Just (expr, reserved)
substDefinitionExpr policy globalEnv env table expr reserved = case expr of
  Lit {} -> Just (expr, reserved)
  Var name ty range
    | isDefined env name -> case policy of
        PreserveExistingPending -> Just (expr, reserved)
        SuspendAtDefinitions ->
          case relevantDefinitionTable globalEnv name table of
            [] -> Just (expr, reserved)
            relevant -> Just (Subst (Var name ty range) relevant, reserved)
    | otherwise -> Just (maybe expr id (lookup name table), reserved)
  Const name _ _ -> Just (maybe expr id (lookup name table), reserved)
  Op {} -> Just (expr, reserved)
  Chain chain -> do
    (chain', reserved') <-
      substDefinitionChain policy globalEnv env table chain reserved
    return (Chain chain', reserved')
  App function argument range -> do
    (function', reserved') <-
      substDefinitionExpr policy globalEnv env table function reserved
    (argument', reserved'') <-
      substDefinitionExpr policy globalEnv env table argument reserved'
    return (App function' argument' range, reserved'')
  Lam binder ty body range -> do
    let (renaming, _, table', env', reserved') =
          prepareLexicalBinders policy env table [binder] [body] reserved
        binder' = renameName renaming binder
        body' = renameFreeOccurrences renaming body
    (body'', reserved'') <-
      substDefinitionExpr policy globalEnv env' table' body' reserved'
    return (Lam binder' ty body'' range, reserved'')
  Tuple exprs -> do
    (exprs', reserved') <-
      substDefinitionExprs policy globalEnv env table exprs reserved
    return (Tuple exprs', reserved')
  OutT index inner -> do
    (inner', reserved') <-
      substDefinitionExpr policy globalEnv env table inner reserved
    return (OutT index inner', reserved')
  Quant op binders range body location -> do
    let names = map fst binders
        tys = map snd binders
        (renaming, names', table', env', reserved') =
          prepareLexicalBinders policy env table names [op, range, body] reserved
        binders' = zip names' tys
        op' = renameFreeOccurrences renaming op
        range' = renameFreeOccurrences renaming range
        body' = renameFreeOccurrences renaming body
    (op'', reserved'') <-
      substDefinitionExpr policy globalEnv env' table' op' reserved'
    (range'', reserved''') <-
      substDefinitionExpr policy globalEnv env' table' range' reserved''
    (body'', reserved'''') <-
      substDefinitionExpr policy globalEnv env' table' body' reserved'''
    return (Quant op'' binders' range'' body'' location, reserved'''')
  ArrIdx array index location -> do
    (array', reserved') <-
      substDefinitionExpr policy globalEnv env table array reserved
    (index', reserved'') <-
      substDefinitionExpr policy globalEnv env table index reserved'
    return (ArrIdx array' index' location, reserved'')
  ArrUpd array index value location -> do
    (array', reserved') <-
      substDefinitionExpr policy globalEnv env table array reserved
    (index', reserved'') <-
      substDefinitionExpr policy globalEnv env table index reserved'
    (value', reserved''') <-
      substDefinitionExpr policy globalEnv env table value reserved''
    return (ArrUpd array' index' value' location, reserved''')
  Case scrutinee clauses location -> do
    (scrutinee', reserved') <-
      substDefinitionExpr policy globalEnv env table scrutinee reserved
    (clauses', reserved'') <-
      substDefinitionClauses policy globalEnv env table clauses reserved'
    return (Case scrutinee' clauses' location, reserved'')
  expression@(Subst subject innerTable)
    | isPendingDefinition env expression -> case policy of
        SuspendAtDefinitions -> Just (Subst expression table, reserved)
        PreserveExistingPending ->
          substPendingRanges policy globalEnv env table expression reserved
    | nestedOpacityConflict policy env table subject innerTable -> Nothing
    | otherwise -> do
        let innerNames = map fst innerTable
            subjectTable =
              tableForSubstitutionSubject policy env table subject innerTable
            captureConflicts =
              tableDomains innerTable
                `Set.intersection` tableRangeFreeVars subjectTable
            (renaming, reserved') = freshenBinders captureConflicts reserved innerNames
            innerNames' = map (renameName renaming) innerNames
            subject' = renameFreeOccurrences renaming subject
        (subject'', reserved'') <-
          substDefinitionExpr policy globalEnv env subjectTable subject' reserved'
        (ranges', reserved''') <-
          substDefinitionExprs
            policy
            globalEnv
            env
            table
            (map snd innerTable)
            reserved''
        return (Subst subject'' (zip innerNames' ranges'), reserved''')
  EHole {} -> Just (expr, reserved)

substDefinitionExprs ::
  DefinitionSubstPolicy ->
  Env ->
  Env ->
  DefinitionSubst ->
  [Expr] ->
  Set Text ->
  Maybe ([Expr], Set Text)
substDefinitionExprs _ _ _ _ [] reserved = Just ([], reserved)
substDefinitionExprs policy globalEnv env table (expr : exprs) reserved = do
  (expr', reserved') <-
    substDefinitionExpr policy globalEnv env table expr reserved
  (exprs', reserved'') <-
    substDefinitionExprs policy globalEnv env table exprs reserved'
  return (expr' : exprs', reserved'')

substDefinitionChain ::
  DefinitionSubstPolicy ->
  Env ->
  Env ->
  DefinitionSubst ->
  Chain ->
  Set Text ->
  Maybe (Chain, Set Text)
substDefinitionChain policy globalEnv env table (Pure expr) reserved = do
  (expr', reserved') <-
    substDefinitionExpr policy globalEnv env table expr reserved
  return (Pure expr', reserved')
substDefinitionChain policy globalEnv env table (More chain op ty expr) reserved = do
  (chain', reserved') <-
    substDefinitionChain policy globalEnv env table chain reserved
  (expr', reserved'') <-
    substDefinitionExpr policy globalEnv env table expr reserved'
  return (More chain' op ty expr', reserved'')

substDefinitionClauses ::
  DefinitionSubstPolicy ->
  Env ->
  Env ->
  DefinitionSubst ->
  [CaseClause] ->
  Set Text ->
  Maybe ([CaseClause], Set Text)
substDefinitionClauses _ _ _ _ [] reserved = Just ([], reserved)
substDefinitionClauses policy globalEnv env table (CaseClause pattern' rhs : clauses) reserved = do
  let names = extractBinder pattern'
      (renaming, _, table', env', reserved') =
        prepareLexicalBinders policy env table names [rhs] reserved
      pattern'' = renamePatternBinders renaming pattern'
      rhs' = renameFreeOccurrences renaming rhs
  (rhs'', reserved'') <-
    substDefinitionExpr policy globalEnv env' table' rhs' reserved'
  (clauses', reserved''') <-
    substDefinitionClauses policy globalEnv env table clauses reserved''
  return (CaseClause pattern'' rhs'' : clauses', reserved''')

substPendingRanges ::
  DefinitionSubstPolicy ->
  Env ->
  Env ->
  DefinitionSubst ->
  Expr ->
  Set Text ->
  Maybe (Expr, Set Text)
substPendingRanges policy globalEnv env table (Subst subject pendingTable) reserved = do
  (subject', reserved') <-
    if isPendingDefinition env subject
      then substPendingRanges policy globalEnv env table subject reserved
      else Just (subject, reserved)
  (ranges', reserved'') <-
    substDefinitionExprs
      policy
      globalEnv
      env
      table
      (map snd pendingTable)
      reserved'
  return (Subst subject' (zip (map fst pendingTable) ranges'), reserved'')
substPendingRanges _ _ _ _ expr reserved = Just (expr, reserved)

substitutionMarkedRedex :: Env -> Expr -> Bool
substitutionMarkedRedex env (Subst subject table) =
  not (isPendingDefinition env subject)
    && not (mustDeferSubstitution SuspendAtDefinitions env table subject)
substitutionMarkedRedex _ _ = False

betaDeferred :: Env -> Name -> Expr -> Expr -> Bool
betaDeferred env binder body argument =
  mustDeferSubstitution
    PreserveExistingPending
    (shadowDefinitions [binder] env)
    [(binder, argument)]
    body

applicationSpineDeferred :: Env -> Expr -> Bool
applicationSpineDeferred env (App function argument _) = case function of
  Lam binder _ body _ -> betaDeferred env binder body argument
  nested@App {} -> applicationSpineDeferred env nested
  _ -> False
applicationSpineDeferred _ _ = False

caseDeferred :: Env -> Expr -> [CaseClause] -> Bool
caseDeferred env scrutinee clauses = case firstMatchingClause scrutinee clauses of
  Nothing -> False
  Just (pattern', rhs, table) ->
    mustDeferSubstitution
      PreserveExistingPending
      (shadowDefinitions (extractBinder pattern') env)
      table
      rhs

firstMatchingClause ::
  Expr ->
  [CaseClause] ->
  Maybe (Pattern, Expr, DefinitionSubst)
firstMatchingClause _ [] = Nothing
firstMatchingClause scrutinee (CaseClause pattern' rhs : clauses) =
  case matchPattern scrutinee pattern' of
    Just table -> Just (pattern', rhs, table)
    Nothing -> firstMatchingClause scrutinee clauses

reduce :: (Fresh m) => Env -> Expr -> Redex -> m Expr
reduce env expr path =
  case definitionAtPath env expr path of
    Nothing -> reduceRawWithEnvs env env expr path
    Just definitionName ->
      let avoid = definitionClosureFreeVars env definitionName
          expr' = alphaRenameAlongPath env avoid expr path
       in reduceRawWithEnvs env env expr' path

reduceRaw :: (Fresh m) => Env -> Expr -> Redex -> m Expr
reduceRaw env = reduceRawWithEnvs env env

-- Keep the complete definition environment separate from the lexically visible
-- one. Definition closures are computed at the definition site from globalEnv;
-- redex visibility and traversal shadowing use env.
reduceRawWithEnvs :: (Fresh m) => Env -> Env -> Expr -> Redex -> m Expr
reduceRawWithEnvs globalEnv env (Chain ch) (i : p) =
  Chain <$> reduceChainWithEnvs globalEnv env ch i p
reduceRawWithEnvs globalEnv env expression@(App (Lam binder _ body _) argument _) []
  | betaDeferred env binder body argument = return expression
  | otherwise = case substWithDefinitions
      PreserveExistingPending
      globalEnv
      (shadowDefinitions [binder] env)
      [(binder, argument)]
      body of
      Just result -> return result
      Nothing -> return expression
reduceRawWithEnvs globalEnv env exp@(App (Var f _ _) e r) [] =
  maybe
    (return exp)
    (\rhs -> reduceRawWithEnvs globalEnv env (App rhs e r) [])
    (lookup f env)
-- The function position is neither a lambda nor a variable. This happens when
-- a point-free definition is inlined into an application, e.g. `id2 = plus 0`
-- or `id2 = case c of ... -> id`, leaving a partial application / `case` /
-- substitution in the function position. Reduce the function position one step
-- so it can progress towards a lambda; if it cannot make progress, leave the
-- application untouched instead of falling through to the catch-all error.
reduceRawWithEnvs globalEnv env exp@(App f e r) []
  | rootReducible env f = do
      f' <- reduceRawWithEnvs globalEnv env f []
      if f' == f
        then return exp
        else reduceRawWithEnvs globalEnv env (App f' e r) []
  | otherwise = return exp
reduceRawWithEnvs globalEnv env (App f e r) (0 : p) =
  App <$> reduceRawWithEnvs globalEnv env f p <*> pure e <*> pure r
reduceRawWithEnvs globalEnv env (App f e r) (1 : p) =
  App f <$> reduceRawWithEnvs globalEnv env e p <*> pure r
reduceRawWithEnvs globalEnv env (Lam x t e r) (0 : p) =
  Lam x t
    <$> reduceRawWithEnvs globalEnv (shadowDefinitions [x] env) e p
    <*> pure r
reduceRawWithEnvs globalEnv env (Tuple es) (n : p) =
  Tuple <$> reduceNthWithEnvs globalEnv env n es p
reduceRawWithEnvs _globalEnv _env (OutT i (Tuple es)) [] = return (es !! i)
reduceRawWithEnvs globalEnv env (OutT i e) (0 : p) =
  OutT i <$> reduceRawWithEnvs globalEnv env e p
reduceRawWithEnvs globalEnv env (Quant op xs ran bdy r) (0 : p) =
  Quant op xs
    <$> reduceRawWithEnvs globalEnv env' ran p
    <*> pure bdy
    <*> pure r
  where
    env' = shadowDefinitions (map fst xs) env
reduceRawWithEnvs globalEnv env (Quant op xs ran bdy r) (1 : p) =
  Quant op xs ran <$> reduceRawWithEnvs globalEnv env' bdy p <*> pure r
  where
    env' = shadowDefinitions (map fst xs) env
reduceRawWithEnvs globalEnv env (ArrIdx a i r) (0 : p) =
  ArrIdx <$> reduceRawWithEnvs globalEnv env a p <*> pure i <*> pure r
reduceRawWithEnvs globalEnv env (ArrIdx a i r) (1 : p) =
  ArrIdx a <$> reduceRawWithEnvs globalEnv env i p <*> pure r
reduceRawWithEnvs globalEnv env (ArrUpd a i e r) (0 : p) =
  ArrUpd
    <$> reduceRawWithEnvs globalEnv env a p
    <*> pure i
    <*> pure e
    <*> pure r
reduceRawWithEnvs globalEnv env (ArrUpd a i e r) (1 : p) =
  ArrUpd a <$> reduceRawWithEnvs globalEnv env i p <*> pure e <*> pure r
reduceRawWithEnvs globalEnv env (ArrUpd a i e r) (2 : p) =
  ArrUpd a i <$> reduceRawWithEnvs globalEnv env e p <*> pure r
reduceRawWithEnvs globalEnv env expr@(Case e cls _) [] =
  maybe expr id
    <$> reduceCaseWithEnvs globalEnv env e cls -- return expr unchanged if cannot reduce
reduceRawWithEnvs globalEnv env (Case e cls r) (0 : p) =
  Case <$> reduceRawWithEnvs globalEnv env e p <*> pure cls <*> pure r
reduceRawWithEnvs globalEnv env (Case e cls r) (n : p) =
  Case e
    <$> reduceNthCaseClauseWithEnvs globalEnv env (n - 1) cls p
    <*> pure r
reduceRawWithEnvs globalEnv env expression@(Subst (Var name _ _) table) []
  | Just rhs <- lookup name env = case substWithDefinitions SuspendAtDefinitions globalEnv env table rhs of
      Just result -> return result
      Nothing -> return expression
reduceRawWithEnvs _globalEnv env expression@(Subst subject _) []
  | isPendingDefinition env subject = return expression
reduceRawWithEnvs _globalEnv env expression@(Subst subject table) []
  | mustDeferSubstitution SuspendAtDefinitions env table subject =
      return expression
reduceRawWithEnvs globalEnv env expression@(Subst subject table) [] =
  case substWithDefinitions SuspendAtDefinitions globalEnv env table subject of
    Just result -> return result
    Nothing -> return expression
reduceRawWithEnvs globalEnv env (Subst e sb) (0 : p) =
  Subst <$> reduceRawWithEnvs globalEnv env e p <*> pure sb
reduceRawWithEnvs globalEnv env (Subst e sb) (n : p) =
  (Subst e . zip (map fst sb))
    <$> reduceNthWithEnvs globalEnv env (n - 1) (map snd sb) p
reduceRawWithEnvs _ _ _ _ = error "shouldn't happen" -- a "catch-all" clause

reduceNth :: (Fresh m) => Env -> Int -> [Expr] -> Redex -> m [Expr]
reduceNth env = reduceNthWithEnvs env env

reduceNthWithEnvs :: (Fresh m) => Env -> Env -> Int -> [Expr] -> Redex -> m [Expr]
reduceNthWithEnvs _ _ _ [] _ = error "shouldn't happen"
reduceNthWithEnvs globalEnv env 0 (e : es) p =
  (: es) <$> reduceRawWithEnvs globalEnv env e p
reduceNthWithEnvs globalEnv env n (e : es) p =
  (e :) <$> reduceNthWithEnvs globalEnv env (n - 1) es p

reduceNthCaseClause :: (Fresh m) => Env -> Int -> [CaseClause] -> Redex -> m [CaseClause]
reduceNthCaseClause env = reduceNthCaseClauseWithEnvs env env

reduceNthCaseClauseWithEnvs ::
  (Fresh m) =>
  Env ->
  Env ->
  Int ->
  [CaseClause] ->
  Redex ->
  m [CaseClause]
reduceNthCaseClauseWithEnvs _ _ _ [] _ = error "shouldn't happen"
reduceNthCaseClauseWithEnvs globalEnv env 0 (CaseClause pattern' rhs : clauses) path =
  (\rhs' -> CaseClause pattern' rhs' : clauses)
    <$> reduceRawWithEnvs
      globalEnv
      (shadowDefinitions (extractBinder pattern') env)
      rhs
      path
reduceNthCaseClauseWithEnvs globalEnv env n (clause : clauses) path =
  (clause :)
    <$> reduceNthCaseClauseWithEnvs globalEnv env (n - 1) clauses path

-- | Whether an expression is reducible at its very root, i.e. whether
--   @reduceRaw env e []@ performs a genuine one-step reduction. Used to decide
--   whether the function position of an application can be simplified before
--   applying.
rootReducible :: Env -> Expr -> Bool
rootReducible env (App (Lam binder _ body _) argument _) =
  not (betaDeferred env binder body argument)
rootReducible env (App (Var name _ _) _ _) = isDefined env name
rootReducible env (Case scrutinee clauses _) =
  not (caseDeferred env scrutinee clauses)
rootReducible env expression@(Subst _ _) =
  not (isPendingDefinition env expression)
    && substitutionMarkedRedex env expression
rootReducible _ (OutT _ (Tuple {})) = True
rootReducible _ _ = False

reduceChain :: (Fresh m) => Env -> Chain -> Int -> Redex -> m Chain
reduceChain env = reduceChainWithEnvs env env

reduceChainWithEnvs ::
  (Fresh m) => Env -> Env -> Chain -> Int -> Redex -> m Chain
reduceChainWithEnvs globalEnv env (Pure e) 0 p =
  Pure <$> reduceRawWithEnvs globalEnv env e p
reduceChainWithEnvs globalEnv env (More ch op t e) 0 p =
  More ch op t <$> reduceRawWithEnvs globalEnv env e p
reduceChainWithEnvs globalEnv env (More ch op t e) i p =
  (\ch' -> More ch' op t e)
    <$> reduceChainWithEnvs globalEnv env ch (i - 1) p
reduceChainWithEnvs _ _ _ _ _ = error "shouldn't happen (reduceChain)"

reduceCase :: (Fresh m) => Env -> Expr -> [CaseClause] -> m (Maybe Expr)
reduceCase env = reduceCaseWithEnvs env env

reduceCaseWithEnvs ::
  (Fresh m) => Env -> Env -> Expr -> [CaseClause] -> m (Maybe Expr)
reduceCaseWithEnvs _globalEnv _env _e [] = return Nothing
reduceCaseWithEnvs globalEnv env e (CaseClause ptn rhs : cls) = do
  case matchPattern e ptn of
    Just table
      | mustDeferSubstitution PreserveExistingPending env' table rhs ->
          return Nothing
      | otherwise ->
          return
            ( substWithDefinitions
                PreserveExistingPending
                globalEnv
                env'
                table
                rhs
            )
      where
        env' = shadowDefinitions (extractBinder ptn) env
    Nothing -> reduceCaseWithEnvs globalEnv env e cls

matchPattern :: Expr -> Pattern -> Maybe DefinitionSubst
matchPattern (Lit l _ _) (PattLit l') | l == l' = Just []
matchPattern e (PattBinder v) = Just [(v, e)]
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
