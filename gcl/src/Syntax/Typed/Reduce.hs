module Syntax.Typed.Reduce where

import Control.Arrow ((***))
import GCL.Common (Fresh (..))
import Syntax.Abstract.Types (Lit (..), Pattern (..))
import Syntax.Common.Types (Name, Op, TypeOp, nameToText)
import Syntax.Substitution
import Syntax.Typed.Instances.Substitution
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
redexes (App f@(Var _ _ _) e _) = [] : map (1 :) (redexes e)
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

redexRT :: Expr -> RT
redexRT (Lit _ _ _) = leaf
redexRT (Var _ _ _) = leaf
redexRT (Const _ _ _) = leaf
redexRT (Op _ _) = leaf
redexRT (Chain ch) = Node False (redexRTChain ch)
redexRT (App f@(Lam _ _ _ _) e _) = Node True [redexRT f, redexRT e]
redexRT (App f@(Var _ _ _) e _) = Node True [leaf, redexRT e]
redexRT (App f e _) = Node False [redexRT f, redexRT e]
redexRT (Lam _ _ e _) = Node False [redexRT e]
redexRT (Tuple es) = Node False (map redexRT es)
redexRT (OutT _ t@(Tuple _)) = Node True [redexRT t]
redexRT (OutT _ e) = Node False [redexRT e]
redexRT (Quant _ _ r b _) = Node False [redexRT r, redexRT b]
redexRT (ArrIdx a i _) = Node False [redexRT a, redexRT i]
redexRT (ArrUpd a i e _) =
  Node False [redexRT a, redexRT i, redexRT e]
redexRT (Case e cls _) =
  Node True (redexRT e : map (redexRT . getClause) cls)
  where
    getClause (CaseClause _ e) = e
redexRT (Subst e sb) =
  Node True (redexRT e : map (redexRT . snd) sb)
redexRT (EHole {}) = leaf

redexRTChain :: Chain -> [RT]
redexRTChain (Pure e) = [redexRT e]
redexRTChain (More ch _ _ e) = redexRT e : redexRTChain ch

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

reduce :: (Fresh m) => Env -> Expr -> Redex -> m Expr
reduce env (Chain ch) (i : p) = Chain <$> reduceChain env ch i p
reduce env (App (Lam x _ bdy _) e _) [] = betaReduce x bdy e
reduce env exp@(App (Var f _ _) e r) [] =
  maybe
    (return exp)
    (\rhs -> reduce env (App rhs e r) [])
    (lookup f env)
reduce env (App f e r) (0 : p) = App <$> reduce env f p <*> pure e <*> pure r
reduce env (App f e r) (1 : p) = App f <$> reduce env e p <*> pure r
reduce env (Lam x t e r) (0 : p) = Lam x t <$> reduce env e p <*> pure r
reduce env (Tuple es) (n : p) = Tuple <$> reduceNth env n es p
reduce env (OutT i (Tuple es)) [] = return (es !! i)
reduce env (OutT i e) (0 : p) = OutT i <$> reduce env e p
reduce env (Quant op xs ran bdy r) (0 : p) =
  Quant op xs <$> reduce env ran p <*> pure bdy <*> pure r
reduce env (Quant op xs ran bdy r) (1 : p) =
  Quant op xs ran <$> reduce env bdy p <*> pure r
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
  (Case e . zipWith CaseClause (map getPattern cls))
    <$> reduceNth env (n - 1) (map getClause cls) p
    <*> pure r
  where
    getPattern (CaseClause p _) = p
    getClause (CaseClause _ e) = e
reduce env (Subst e sb) [] = subst (map (nameToText *** id) sb) e
reduce env (Subst e sb) (0 : p) = Subst <$> reduce env e p <*> pure sb
reduce env (Subst e sb) (n : p) =
  (Subst e . zip (map fst sb))
    <$> reduceNth env (n - 1) (map snd sb) p
reduce _ _ _ = error "shouldn't happen" -- a "catch-all" clause

reduceNth :: (Fresh m) => Env -> Int -> [Expr] -> Redex -> m [Expr]
reduceNth _ _ [] _ = error "shouldn't happen"
reduceNth env 0 (e : es) p = (: es) <$> reduce env e p
reduceNth env n (e : es) p = (e :) <$> reduceNth env (n - 1) es p

betaReduce :: (Fresh m) => Name -> Expr -> Expr -> m Expr
betaReduce x bdy e = subst [(nameToText x, e)] bdy

reduceChain :: (Fresh m) => Env -> Chain -> Int -> Redex -> m Chain
reduceChain env (Pure e) 0 p = Pure <$> reduce env e p
reduceChain env (More ch op t e) 0 p = More ch op t <$> reduce env e p
reduceChain env (More ch op t e) i p =
  (\ch' -> More ch' op t e) <$> reduceChain env ch (i - 1) p
reduceChain _ _ _ _ = error "shouldn't happen (reduceChain)"

reduceCase :: (Fresh m) => Env -> Expr -> [CaseClause] -> m (Maybe Expr)
reduceCase env e [] = return Nothing
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
