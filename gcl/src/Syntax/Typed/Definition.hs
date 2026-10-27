-- | Make the state read by an unexpanded definition explicit.
--
-- A reference @f@ that reads globals @x, y@ is represented as
-- @f[x, y \ x, y]@. Program substitution updates these values; ordinary
-- beta substitution leaves the definition's globals alone. Keeping identity
-- entries is essential: they expose hidden free names to capture avoidance
-- and let a renamed outer table still update the original global.
--
-- Lexical binders are renamed before these environments are introduced, so
-- their local occurrences cannot accidentally become definition globals.
module Syntax.Typed.Definition
  ( DefnEnv,
    DefinitionContext,
    definitionContext,
    definitionEnv,
    closeDefinitions,
    hiddenGlobals,
    binderRenaming,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GCL.Common (Fresh, freshAvoiding)
import Syntax.Abstract.Types (extractBinder)
import Syntax.Common.Types (Name, nameToText)
import Syntax.Typed.Subst2 (allNames, renameFree, renameName, renamePatternBinders)
import Syntax.Typed.Types

type DefnEnv = [(Name, Expr)]

type Globals = Map Name Expr

data DefinitionContext = DefinitionContext
  { definitionEnv :: DefnEnv,
    globals :: Map Name Globals,
    definitionNames :: Set Text
  }

-- | Compute transitive dependencies without unfolding expressions. A fixed
-- point handles recursive definitions, and canonical occurrences prevent
-- source ranges from oscillating between mutually recursive bodies.
definitionContext :: DefnEnv -> DefinitionContext
definitionContext env = DefinitionContext env (fixedPoint Map.empty) forbidden
  where
    direct = foldMap (freeGlobals env Map.empty Set.empty . snd) env
    forbidden = Set.fromList (map (nameToText . fst) env) <> foldMap (allNames . snd) env
    step known =
      Map.fromList
        [ (name, Map.restrictKeys direct (Map.keysSet (freeGlobals env known Set.empty body)))
        | (name, body) <- env
        ]
    fixedPoint known =
      let next = step known
       in if next == known then known else fixedPoint next

freeGlobals :: DefnEnv -> Map Name Globals -> Set Name -> Expr -> Globals
freeGlobals env known bound e@(Var name _ _)
  | name `Set.member` bound = Map.empty
  | isJust (lookup name env) = Map.findWithDefault Map.empty name known
  | otherwise = Map.singleton name e
freeGlobals _ _ bound e@(Const name _ _)
  | name `Set.member` bound = Map.empty
  | otherwise = Map.singleton name e
freeGlobals env known bound (Lam name _ body _) =
  freeGlobals env known (Set.insert name bound) body
freeGlobals env known bound (Quant operator binders range body _) =
  freeGlobals env known bound operator
    <> freeGlobals env known bound' range
    <> freeGlobals env known bound' body
  where
    bound' = bound <> Set.fromList (map fst binders)
freeGlobals env known bound (Case scrutinee clauses _) =
  freeGlobals env known bound scrutinee
    <> foldMap clauseGlobals clauses
  where
    clauseGlobals (CaseClause pattern' body) =
      freeGlobals env known (bound <> Set.fromList (extractBinder pattern')) body
freeGlobals env known bound (Subst body table) =
  Map.withoutKeys (freeGlobals env known (bound <> domain) body) domain
    <> foldMap (freeGlobals env known bound . snd) table
  where
    domain = Set.fromList (map fst table)
freeGlobals env known bound expression =
  foldMap (freeGlobals env known bound) (children expression)

-- | Names that will become explicit when references in this expression are
-- closed. Lexical binders hide definitions with the same name, but do not
-- bind globals read inside another definition. A program table does bind
-- those globals in its subject; its values are outside that scope.
hiddenGlobals :: DefinitionContext -> DefnEnv -> Expr -> Set Text
hiddenGlobals context env (Var name _ _)
  | isJust (lookup name env) =
      Set.map nameToText (Map.keysSet (Map.findWithDefault Map.empty name (globals context)))
hiddenGlobals context env (Lam name _ body _) =
  hiddenGlobals context (shadow [name] env) body
hiddenGlobals context env (Quant operator binders range body _) =
  hiddenGlobals context env operator
    <> hiddenGlobals context env' range
    <> hiddenGlobals context env' body
  where
    env' = shadow (map fst binders) env
hiddenGlobals context env (Case scrutinee clauses _) =
  hiddenGlobals context env scrutinee <> foldMap clauseGlobals clauses
  where
    clauseGlobals (CaseClause pattern' body) =
      hiddenGlobals context (shadow (extractBinder pattern') env) body
hiddenGlobals context env (Subst body table) =
  (hiddenGlobals context (shadow domain env) body Set.\\ Set.fromList (map nameToText domain))
    <> foldMap (hiddenGlobals context env . snd) table
  where
    domain = map fst table
hiddenGlobals context env expression =
  foldMap (hiddenGlobals context env) (children expression)

-- | Introduce environments without unfolding any definition. The visible
-- environment belongs to the expression's current lexical scope; the
-- dependency information always belongs to the definitions' declaration
-- scope.
closeDefinitions :: (Fresh m) => DefinitionContext -> DefnEnv -> Expr -> m Expr
closeDefinitions context = go
  where
    go env expression@(Var name _ _)
      | isJust (lookup name env),
        let table = Map.toList (Map.findWithDefault Map.empty name (globals context)),
        not (null table) =
          pure (Subst expression table)
    go _ expression@Lit {} = pure expression
    go _ expression@Var {} = pure expression
    go _ expression@Const {} = pure expression
    go _ expression@Op {} = pure expression
    go env (Chain chain) = Chain <$> goChain env chain
    go env (App function argument range) =
      App <$> go env function <*> go env argument <*> pure range
    go env expression@(Lam name t body range) = do
      let env' = shadow [name] env
      renaming <- binderRenaming context expression [name] (hiddenGlobals context env' body)
      body' <- go env' (renameFree renaming body)
      pure (Lam (renameName renaming name) t body' range)
    go env (Tuple elements) = Tuple <$> mapM (go env) elements
    go env (OutT index expression) = OutT index <$> go env expression
    go env expression@(Quant operator binders range body location) = do
      let names = map fst binders
          env' = shadow names env
          incoming = hiddenGlobals context env' range <> hiddenGlobals context env' body
      renaming <- binderRenaming context expression names incoming
      Quant
        <$> go env operator
        <*> pure [(renameName renaming name, t) | (name, t) <- binders]
        <*> go env' (renameFree renaming range)
        <*> go env' (renameFree renaming body)
        <*> pure location
    go env (ArrIdx array index range) =
      ArrIdx <$> go env array <*> go env index <*> pure range
    go env (ArrUpd array index value range) =
      ArrUpd <$> go env array <*> go env index <*> go env value <*> pure range
    go env expression@(Case scrutinee clauses range) =
      Case <$> go env scrutinee <*> mapM (goClause env expression) clauses <*> pure range
    go env (Subst subject table) = do
      table' <- mapM (\(name, value) -> (,) name <$> go env value) table
      let names = map fst table
      case subject of
        Var name _ _
          | name `notElem` names,
            isJust (lookup name env) -> do
              let missing = Map.withoutKeys (Map.findWithDefault Map.empty name (globals context)) (Set.fromList names)
              pure (Subst subject (table' <> Map.toList missing))
        _ -> Subst <$> go (shadow names env) subject <*> pure table'
    go _ expression@EHole {} = pure expression

    goChain env (Pure expression) = Pure <$> go env expression
    goChain env (More chain operator t expression) =
      More <$> goChain env chain <*> pure operator <*> pure t <*> go env expression

    goClause env region (CaseClause pattern' body) = do
      let names = extractBinder pattern'
          env' = shadow names env
      renaming <- binderRenaming context region names (hiddenGlobals context env' body)
      CaseClause
        (renamePatternBinders renaming pattern')
        <$> go env' (renameFree renaming body)

-- | Choose fresh names for lexical binders that would capture newly exposed
-- globals. Definition names and all names in their bodies are forbidden too,
-- so renaming cannot change which future references resolve to definitions.
binderRenaming :: (Fresh m) => DefinitionContext -> Expr -> [Name] -> Set Text -> m [(Text, Text)]
binderRenaming context region binders incoming = allocate forbidden clashing
  where
    clashing = filter (\name -> nameToText name `Set.member` incoming) binders
    forbidden = definitionNames context <> allNames region <> incoming
    allocate _ [] = pure []
    allocate used (name : rest) = do
      target <- freshAvoiding used (nameToText name)
      ((nameToText name, target) :) <$> allocate (Set.insert target used) rest

shadow :: [Name] -> DefnEnv -> DefnEnv
shadow names = filter (\(name, _) -> name `notElem` names)

children :: Expr -> [Expr]
children Lit {} = []
children Var {} = []
children Const {} = []
children Op {} = []
children (Chain chain) = chainChildren chain
children (App function argument _) = [function, argument]
children (Lam _ _ body _) = [body]
children (Tuple elements) = elements
children (OutT _ expression) = [expression]
children (Quant operator _ range body _) = [operator, range, body]
children (ArrIdx array index _) = [array, index]
children (ArrUpd array index value _) = [array, index, value]
children (Case scrutinee clauses _) = scrutinee : [body | CaseClause _ body <- clauses]
children (Subst body table) = body : map snd table
children EHole {} = []

chainChildren :: Chain -> [Expr]
chainChildren (Pure expression) = [expression]
chainChildren (More chain _ _ expression) = expression : chainChildren chain
