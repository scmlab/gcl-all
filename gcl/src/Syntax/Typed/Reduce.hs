module Syntax.Typed.Reduce where

import Control.Arrow ((***))
import GCL.Common (Fresh (..))
import Syntax.Abstract.Types (Pattern (..), extractBinder)
import Syntax.Common.Types (Name, nameToText)
import Syntax.Substitution
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
-- Quant children follow constructor order: operator, restriction, body.
redexes (Quant op _ r b _) =
  map (0 :) (redexes op)
    ++ map (1 :) (redexes r)
    ++ map (2 :) (redexes b)
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
redexRT env (Quant op xs r b _) =
  Node False [redexRT env op, redexRT env' r, redexRT env' b]
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
redexRT_sat env (Quant op xs r b _) =
  Node False [redexRT_sat env op, redexRT_sat env' r, redexRT_sat env' b]
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
  where
    applicationHead (App function _ _) = applicationHead function
    applicationHead e = e

reduce :: (Fresh m) => Env -> Expr -> Redex -> m Expr
reduce env (Chain ch) (i : p) = Chain <$> reduceChain env ch i p
reduce _env (App (Lam x _ bdy _) e _) [] = betaReduce x bdy e
reduce env exp@(App (Var f _ _) e r) [] =
  maybe
    (return exp)
    (\rhs -> reduce env (App rhs e r) [])
    (lookup f env)
-- The function position is neither a lambda nor a variable. This happens when
-- a point-free definition is inlined into an application, e.g. `id2 = plus 0`
-- or `id2 = case c of ... -> id`, leaving a partial application / `case` /
-- substitution in the function position. Reduce the function position one step
-- so it can progress towards a lambda; if it cannot make progress, leave the
-- application untouched instead of falling through to the catch-all error.
reduce env exp@(App f e r) []
  | rootReducible f = do
      f' <- reduce env f []
      if f' == f then return exp else reduce env (App f' e r) []
  | otherwise = return exp
reduce env (App f e r) (0 : p) = App <$> reduce env f p <*> pure e <*> pure r
reduce env (App f e r) (1 : p) = App f <$> reduce env e p <*> pure r
reduce env (Lam x t e r) (0 : p) = Lam x t <$> reduce (shadowDefinitions [x] env) e p <*> pure r
reduce env (Tuple es) (n : p) = Tuple <$> reduceNth env n es p
reduce _env (OutT i (Tuple es)) [] = return (es !! i)
reduce env (OutT i e) (0 : p) = OutT i <$> reduce env e p
reduce env (Quant op xs ran bdy r) (0 : p) =
  (\op' -> Quant op' xs ran bdy r) <$> reduce env op p
reduce env (Quant op xs ran bdy r) (1 : p) =
  Quant op xs <$> reduce env' ran p <*> pure bdy <*> pure r
  where
    env' = shadowDefinitions (map fst xs) env
reduce env (Quant op xs ran bdy r) (2 : p) =
  Quant op xs ran <$> reduce env' bdy p <*> pure r
  where
    env' = shadowDefinitions (map fst xs) env
reduce env (ArrIdx a i r) (0 : p) = ArrIdx <$> reduce env a p <*> pure i <*> pure r
reduce env (ArrIdx a i r) (1 : p) = ArrIdx a <$> reduce env i p <*> pure r
reduce env (ArrUpd a i e r) (0 : p) =
  ArrUpd <$> reduce env a p <*> pure i <*> pure e <*> pure r
reduce env (ArrUpd a i e r) (1 : p) = ArrUpd a <$> reduce env i p <*> pure e <*> pure r
reduce env (ArrUpd a i e r) (2 : p) = ArrUpd a i <$> reduce env e p <*> pure r
reduce env expr@(Case e cls _) [] =
  maybe expr id <$> reduceCase env e cls -- return expr unchanged if cannot reduce
reduce env (Case e cls r) (0 : p) =
  Case <$> reduce env e p <*> pure cls <*> pure r
reduce env (Case e cls r) (n : p) =
  Case e <$> reduceNthCaseClause env (n - 1) cls p <*> pure r
reduce _env (Subst e sb) [] = subst (map (nameToText *** id) sb) e
reduce env (Subst e sb) (0 : p) = Subst <$> reduce env e p <*> pure sb
reduce env (Subst e sb) (n : p) =
  (Subst e . zip (map fst sb))
    <$> reduceNth env (n - 1) (map snd sb) p
reduce _ _ _ = error "shouldn't happen" -- a "catch-all" clause

reduceNth :: (Fresh m) => Env -> Int -> [Expr] -> Redex -> m [Expr]
reduceNth _ _ [] _ = error "shouldn't happen"
reduceNth env 0 (e : es) p = (: es) <$> reduce env e p
reduceNth env n (e : es) p = (e :) <$> reduceNth env (n - 1) es p

reduceNthCaseClause :: (Fresh m) => Env -> Int -> [CaseClause] -> Redex -> m [CaseClause]
reduceNthCaseClause _ _ [] _ = error "shouldn't happen"
reduceNthCaseClause env 0 (CaseClause pattern' rhs : clauses) path =
  (\rhs' -> CaseClause pattern' rhs' : clauses)
    <$> reduce (shadowDefinitions (extractBinder pattern') env) rhs path
reduceNthCaseClause env n (clause : clauses) path =
  (clause :) <$> reduceNthCaseClause env (n - 1) clauses path

betaReduce :: (Fresh m) => Name -> Expr -> Expr -> m Expr
betaReduce x bdy e = subst [(nameToText x, e)] bdy

-- | Whether an expression is reducible at its very root, i.e. whether
--   @reduce env e []@ performs a genuine one-step reduction. Used to decide
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
reduceChain env (Pure e) 0 p = Pure <$> reduce env e p
reduceChain env (More ch op t e) 0 p = More ch op t <$> reduce env e p
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
