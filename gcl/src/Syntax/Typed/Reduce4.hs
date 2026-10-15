-- | One-click and bounded full reduction over ordinary typed expressions.
--
-- Lexical binders receive private, unique names while a click is evaluated.
-- Consequently beta/case substitution cannot capture a lexical variable, and
-- program-state substitution can distinguish globals using names alone.
-- Output restores the original binder names wherever they remain unambiguous.
--
-- Definition bodies retain declaration scope. State propagation treats their
-- references as opaque and keeps updates without following dependencies.
-- Pending Substs retain their own click except during lambda repair.
-- Types and ranges survive renaming; holes and their source Envs stay opaque.
module Syntax.Typed.Reduce4
  ( DefnEnv,
    RedexPath,
    RTree (..),
    RT,
    tryReduceAt,
    reduceAll,
    redexRT,
  )
where

import Control.Monad (foldM)
import Control.Monad.State (StateT, evalState, get, modify, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (bimap)
import Data.List ((!?))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GCL.Common (Fresh, freshAvoiding, initFreshState)
import Syntax.Abstract.Types (Pattern (..), extractBinder)
import Syntax.Common.Types (Name (..), nameToText)
import Syntax.Typed.Structure
import Syntax.Typed.Types

type DefnEnv = [(Name, Expr)]

type Substitution = [(Name, Expr)]

type Renaming = [(Text, Text)]

-- The only extra state is the definition environment and the display name of
-- each private binder. There is no second expression tree or dependency table.
data Context = Context
  { bodies :: Map Name Expr,
    originalNames :: Map Text Text,
    forbidden :: Set Text
  }

-- Frames run from the immediate parent of the selection to the root.
type Frame = (Int, Expr)

tryReduceAt :: (Fresh m) => DefnEnv -> Expr -> RedexPath -> m (Maybe Expr)
tryReduceAt env expression path = do
  (context, prepared) <- prepare env expression
  case focus prepared path of
    Nothing -> pure Nothing
    Just (selected, frames) -> reduceFocused context expression selected frames

-- | Reduce the first redex in root-first 'pathChildren' order until none remain.
-- The limit counts successful one-click reductions, including self-unfolds.
-- 'Right' contains an expression without redexes. It may still be stuck, e.g.
-- on a definition that is never applied; 'Left' contains the unfinished one.
-- Non-positive limits permit no steps, but an irreducible input still succeeds.
-- Steps keep private binder names, so the result restores the input's names
-- once, rather than inheriting a fallback name some intermediate step needed.
-- That restore counts from zero again, so fallback names do not grow with the
-- number of steps. They avoid every name in the final expression; private
-- names of earlier steps no longer occur and need not be avoided.
reduceAll :: Int -> DefnEnv -> Expr -> Either Expr Expr
reduceAll limit env expression =
  bimap finish finish (evalState (prepare env expression >>= uncurry (go limit)) initFreshState)
  where
    finish (context, current) =
      evalState (restore context {forbidden = allNames current} current) initFreshState
    go remaining context current = do
      next <- reduceOnce context current
      case next of
        Nothing -> pure (Right (context, current))
        Just result
          | remaining <= 0 -> pure (Left (context, current))
          | otherwise -> do
              (context', prepared) <- prepareWith (originalNames context) env result
              go (remaining - 1) context' prepared

-- Search a prepared expression. A Subst that recreates itself waits for its
-- reference, as in 'reduceFocused', but is compared before restoring names.
reduceOnce :: (Fresh m) => Context -> Expr -> m (Maybe Expr)
reduceOnce context expression = search [(expression, [])]
  where
    search [] = pure Nothing
    search ((selected, frames) : pending) = do
      result <- stepFocused context selected frames
      case (selected, result) of
        (Subst {}, Just same) | same == expression -> deeper
        (_, Just _) -> pure result
        (_, Nothing) -> deeper
      where
        deeper =
          search $
            [(child, (index, selected) : frames) | (index, child) <- zip [0 ..] (pathChildren selected)]
              <> pending

redexRT :: DefnEnv -> Expr -> RT
redexRT env expression = go [] prepared
  where
    (context, prepared) = evalState (prepare env expression) initFreshState
    go frames selected =
      Node
        (isJust (evalState (reduceFocused context expression selected frames) initFreshState))
        [go ((index, selected) : frames) child | (index, child) <- zip [0 ..] (pathChildren selected)]

reduceFocused :: (Fresh m) => Context -> Expr -> Expr -> [Frame] -> m (Maybe Expr)
reduceFocused context original selected frames = do
  result <- traverse (restore context) =<< stepFocused context selected frames
  pure $ case selected of
    -- A reference's environment waits for its unfold when applying it would
    -- recreate the same displayed Subst. Self-reference unfolds still count.
    Subst {} | result == Just original -> Nothing
    _ -> result

-- One rule at a prepared selection; the result keeps private binder names.
stepFocused :: (Fresh m) => Context -> Expr -> [Frame] -> m (Maybe Expr)
stepFocused context selected frames = case selected of
  Var name _ _
    | Just body <- definition context (hiddenDefinitions frames) name -> repair context body frames
  _ -> fmap (flip rebuild frames) <$> localStep context (hiddenDefinitions frames) selected

repair :: (Fresh m) => Context -> Expr -> [Frame] -> m (Maybe Expr)
-- Defensive only: elaborated definition bodies contain no Subst, and repair
-- adds Subst only over references, so a replacement never wraps a Lam here.
repair context (Subst subject assignments) frames
  | leadingLambda subject = do
      result <- applyState context (hiddenDefinitions frames <> domain assignments) assignments subject
      repair context result frames
repair context replacement ((0, Subst _ assignments) : frames)
  | leadingLambda replacement = do
      result <- applyState context (hiddenDefinitions frames <> domain assignments) assignments replacement
      repair context result frames
repair context (Lam name _ body _) ((0, App _ argument _) : frames) = do
  result <- replaceLocals context (hiddenDefinitions frames) [(name, argument)] body
  repair context result frames
repair _ Lam {} _ = pure Nothing
repair _ replacement frames = pure (Just (rebuild replacement frames))

leadingLambda :: Expr -> Bool
leadingLambda Lam {} = True
leadingLambda (Subst subject _) = leadingLambda subject
leadingLambda _ = False

focus :: Expr -> RedexPath -> Maybe (Expr, [Frame])
focus expression [] = Just (expression, [])
focus expression (index : rest) = do
  child <- pathChildren expression !? index
  (selected, frames) <- focus child rest
  pure (selected, frames <> [(index, expression)])

rebuild :: Expr -> [Frame] -> Expr
rebuild = foldl plug
  where
    plug replacement (index, parent) =
      case replaceAt index replacement (pathChildren parent) of
        Just children -> withPathChildren parent children
        Nothing -> error "Reduce4: invalid saved frame"

hiddenDefinitions :: [Frame] -> Set Name
hiddenDefinitions frames = Set.unions [domain table | (0, Subst _ table) <- frames]

definition :: Context -> Set Name -> Name -> Maybe Expr
definition context hidden name
  | Set.member name hidden = Nothing
  | otherwise = Map.lookup name (bodies context)

localStep :: (Fresh m) => Context -> Set Name -> Expr -> m (Maybe Expr)
localStep _ _ (OutT index (Tuple elements)) = pure (elements !? index)
localStep context hidden (Subst subject assignments) =
  Just <$> applyState context (hidden <> domain assignments) assignments subject
localStep context hidden (Case scrutinee clauses _) = choose clauses
  where
    choose [] = pure Nothing
    choose (CaseClause pattern' body : rest) = case match scrutinee pattern' of
      Matched assignments -> Just <$> replaceLocals context hidden assignments body
      Refuted -> choose rest
      Undecided -> pure Nothing
localStep _ _ _ = pure Nothing

data Match = Matched Substitution | Refuted | Undecided

match :: Expr -> Pattern -> Match
match expression (PattBinder name) = Matched [(name, expression)]
match _ (PattWildcard _) = Matched []
match (Lit literal _ _) (PattLit expected)
  | literal == expected = Matched []
  | otherwise = Refuted
match _ (PattLit _) = Undecided
match (Tuple elements) (PattTuple patterns)
  | length elements == length patterns = foldr combine (Matched []) (zipWith match elements patterns)
  | otherwise = Refuted
match Lit {} (PattTuple _) = Refuted
match _ (PattTuple _) = Undecided
match _ (PattConstructor _ _) = Undecided

combine :: Match -> Match -> Match
combine Refuted _ = Refuted
combine _ Refuted = Refuted
combine Undecided _ = Undecided
combine _ Undecided = Undecided
combine (Matched left) (Matched right) = Matched (left <> right)

-- State values are simultaneous. At an existing Subst, update only its values
-- and compose the tables; leave execution of the subject for another click.
-- An unexpanded reference keeps every non-identity update until it unfolds.
applyState :: (Fresh m) => Context -> Set Name -> Substitution -> Expr -> m Expr
applyState _ _ [] expression = pure expression
applyState context hidden table expression@(Var name _ _)
  | Just _ <- definition context hidden name =
      pure $ case filter (not . identityEntry context hidden) table of
        [] -> expression
        pending -> Subst expression pending
applyState context _ table expression@(Var name _ _)
  | not (privateName context name) = pure (maybe expression id (lookup name table))
applyState context _ table expression@(Const name _ _)
  | not (privateName context name) = pure (maybe expression id (lookup name table))
applyState context hidden table (Subst subject earlier) = do
  values <- mapM (\(key, value) -> (,) key <$> applyState context hidden table value) earlier
  let remaining =
        [ entry
        | entry@(key, _) <- table,
          Set.notMember key (domain earlier),
          mayReadState context (hidden <> domain earlier) key subject
        ]
  pure (Subst subject (values <> remaining))
applyState context hidden table expression =
  withPathChildren expression <$> mapM (applyState context hidden table) (pathChildren expression)

-- Private binder names make ordinary replacement capture-free. A state table
-- is a different boundary: inserting caller x into (t + x)[x := 5] must defer
-- t's replacement into a fresh simultaneous slot, rather than update that x.
replaceLocals :: (Fresh m) => Context -> Set Name -> Substitution -> Expr -> m Expr
replaceLocals _ _ [] expression = pure expression
replaceLocals _ _ table expression@(Var name _ _) = pure (maybe expression id (lookup name table))
replaceLocals _ _ table expression@(Const name _ _) = pure (maybe expression id (lookup name table))
replaceLocals context hidden table expression@(Subst subject earlier) = do
  let active = [(key, value) | (key, value) <- table, Set.member key (freeNames subject)]
      clashes = [(key, value) | (key, value) <- active, any (exposed value) (map fst earlier)]
      exposed value key = Set.member key (freeNames value) || mayReadState context hidden key value
      avoid = forbidden context <> allNames expression <> foldMap (allNames . snd) table
  renaming <- slots avoid (map fst clashes)
  subject' <- replaceLocals context (hidden <> domain earlier) table (renameVariables renaming subject)
  values <- mapM (\(key, value) -> (,) key <$> replaceLocals context hidden table value) earlier
  pure (Subst subject' (values <> [(renameName renaming key, value) | (key, value) <- clashes]))
  where
    slots _ [] = pure []
    slots avoid (key : rest) = do
      target <- freshAvoiding avoid (preferredName context key)
      ((nameToText key, target) :) <$> slots (Set.insert target avoid) rest
replaceLocals context hidden table expression =
  withPathChildren expression <$> mapM (replaceLocals context hidden table) (pathChildren expression)

privateName :: Context -> Name -> Bool
privateName context name = Map.member (nameToText name) (originalNames context)

preferredName :: Context -> Name -> Text
preferredName context name = Map.findWithDefault (nameToText name) (nameToText name) (originalNames context)

identityEntry :: Context -> Set Name -> (Name, Expr) -> Bool
identityEntry context hidden (key, Var name _ _) =
  key == name && not (privateName context name) && not (isJust (definition context hidden name))
identityEntry context _ (key, Const name _ _) = key == name && not (privateName context name)
identityEntry _ _ _ = False

domain :: Substitution -> Set Name
domain = Set.fromList . map fst

-- Inspect the displayed Expr only. A definition reference may read any global;
-- its body is neither visited nor followed through aliases before unfolding.
mayReadState :: Context -> Set Name -> Name -> Expr -> Bool
mayReadState context hidden key (Var name _ _)
  | privateName context name = False
  | isJust (definition context hidden name) = True
  | otherwise = name == key
mayReadState context _ key (Const name _ _) = not (privateName context name) && name == key
mayReadState context hidden key (Subst subject table) =
  (Set.notMember key (domain table) && mayReadState context hidden' key subject)
    || any (\(target, value) -> mayReadState context hidden' target subject && mayReadState context hidden key value) table
  where
    hidden' = hidden <> domain table
mayReadState context hidden key expression = any (mayReadState context hidden key) (pathChildren expression)

prepare :: (Fresh m) => DefnEnv -> Expr -> m (Context, Expr)
prepare = prepareWith Map.empty

-- 'inherited' maps private names already in the expression to the names they
-- should restore to, so repeated steps never treat a private name as original.
prepareWith :: (Fresh m) => Map Text Text -> DefnEnv -> Expr -> m (Context, Expr)
prepareWith inherited env expression = do
  ((compiled, prepared), names) <- runStateT action Map.empty
  pure (Context (Map.fromList compiled) names (avoid <> Map.keysSet names), prepared)
  where
    avoid = allNames expression <> foldMap (allNames . snd) env <> Set.fromList (map (nameToText . fst) env)
    origin name = Map.findWithDefault name name inherited
    action = do
      compiled <- mapM (\(name, body) -> (,) name <$> freshen id avoid body) env
      prepared <- freshen origin avoid expression
      pure (compiled, prepared)

-- Alpha-convert lexical binders only. A Subst domain still denotes the
-- original program globals; it never renames a lexical binding.
freshen :: (Fresh m) => (Text -> Text) -> Set Text -> Expr -> StateT (Map Text Text) m Expr
freshen origin avoid expression = do
  (header, children) <- underScopes allocate expression
  withPathChildren header <$> mapM (freshen origin avoid) children
  where
    allocate names _ = mapM allocateName names
    allocateName :: (Fresh n) => Name -> StateT (Map Text Text) n (Text, Text)
    allocateName name = do
      originals <- get
      let original = origin (nameToText name)
      target <- lift (freshAvoiding (avoid <> Map.keysSet originals) original)
      modify (Map.insert target original)
      pure (nameToText name, target)

-- Undo private alpha-conversion. Keep a binder's original name unless it
-- would hide a displayed global/reference, a state key, or an outer local.
restore :: (Fresh m) => Context -> Expr -> m Expr
restore context expression = do
  (header, children) <- underScopes choose expression
  withPathChildren header <$> mapM (restore context) children
  where
    choose names region = allocate avoid names
      where
        protected = foldMap freeNames region Set.\\ Set.fromList names
        avoid = forbidden context <> foldMap allNames region
        allocate _ [] = pure []
        allocate used (name : rest) = do
          let preferred = preferredName context name
          target <-
            if Set.member (Name preferred Nothing) protected
              then freshAvoiding used preferred
              else pure preferred
          ((nameToText name, target) :) <$> allocate (Set.insert target used) rest

-- These names are visible outside lexical scopes. State keys count because
-- restoring a same-named binder would obscure the pending global operation.
freeNames :: Expr -> Set Name
freeNames (Var name _ _) = Set.singleton name
freeNames (Const name _ _) = Set.singleton name
freeNames expression@(Subst _ table) = domain table <> foldMap freeNames (pathChildren expression)
freeNames expression =
  Set.unions $
    zipWith
      (\names child -> freeNames child Set.\\ Set.fromList names)
      (lexicalBinders expression)
      (pathChildren expression)

renameVariables :: Renaming -> Expr -> Expr
renameVariables renaming (Var name t range) = Var (renameName renaming name) t range
renameVariables renaming (Const name t range) = Const (renameName renaming name) t range
renameVariables renaming expression =
  withPathChildren expression (zipWith under (lexicalBinders expression) (pathChildren expression))
  where
    under names = renameVariables (filter (\(key, _) -> notElem key (map nameToText names)) renaming)

lexicalBinders :: Expr -> [[Name]]
lexicalBinders expression@Subst {} = map (const []) (pathChildren expression)
lexicalBinders expression = childBinders expression

-- Freshening and display restoration use the same lexical-scope traversal.
-- Quantifier restriction/body share one choice; each case clause is separate.
underScopes :: (Monad m) => ([Name] -> [Expr] -> m Renaming) -> Expr -> m (Expr, [Expr])
underScopes choose expression = foldM under (expression, pathChildren expression) (scopeGroups expression)
  where
    under (header, children) (index, names, positions) = do
      renaming <- choose names [children !! position | position <- positions]
      let renamed =
            [ if elem position positions then renameVariables renaming child else child
            | (position, child) <- zip [0 ..] children
            ]
      pure (renameHeader index renaming header, renamed)

scopeGroups :: Expr -> [(Int, [Name], [Int])]
scopeGroups (Lam name _ _ _) = [(0, [name], [0])]
scopeGroups (Quant _ binders _ _ _) = [(1, map fst binders, [1, 2])]
scopeGroups (Case _ clauses _) = [(index, extractBinder pattern', [index]) | (index, CaseClause pattern' _) <- zip [1 ..] clauses]
scopeGroups _ = []

renameHeader :: Int -> Renaming -> Expr -> Expr
renameHeader _ renaming (Lam name t body range) = Lam (renameName renaming name) t body range
renameHeader _ renaming (Quant op binders restriction body range) =
  Quant op [(renameName renaming name, t) | (name, t) <- binders] restriction body range
renameHeader index renaming (Case scrutinee clauses range) =
  Case
    scrutinee
    [ if position == index then CaseClause (renamePatternBinders renaming pattern') body else clause
    | (position, clause@(CaseClause pattern' body)) <- zip [1 ..] clauses
    ]
    range
renameHeader _ _ expression = expression
