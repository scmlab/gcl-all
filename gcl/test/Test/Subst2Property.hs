{-# LANGUAGE OverloadedStrings #-}

-- | Structural differential properties for capture-avoiding substitution.
--
-- Routine runs use at least 1000 cases per property. For a deeper run:
--
-- @
-- stack test --test-arguments='--pattern "Subst2 properties" --quickcheck-tests=40000'
-- @
module Test.Subst2Property (tests) where

import Control.Monad.State
import Data.List (nub)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GCL.Common (freeVarsT)
import GCL.Range (Range, mkPos, mkRange)
import GCL.WP (runWP)
import qualified Syntax.Abstract.Types as A
import Syntax.Common.Types (ArithOp (..), Name (..), Op (..), nameToText)
import Syntax.Concrete.Instances.ToAbstract ()
import Syntax.Typed.Instances.Free ()
import Syntax.Typed.Subst2 (renameForSubstitution, substExpr)
import qualified Syntax.Typed.Types as T
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck

-- a deliberately tiny pool, including names WP's freshPreInScope would invent
pool :: [Text]
pool = ["a", "b", "c", "a0", "a'"]

tyInt, tyBool :: A.Type
tyInt = A.TBase A.TInt Nothing
tyBool = A.TBase A.TBool Nothing

genRange :: Gen (Maybe Range)
genRange = elements [Nothing, Just (mkRange (mkPos 1 1) (mkPos 1 2)), Just (mkRange (mkPos 2 3) (mkPos 2 9))]

genName :: Gen Name
genName = Name <$> elements pool <*> genRange

genType :: Gen A.Type
genType = elements [tyInt, tyBool]

genLit :: Gen A.Lit
genLit = elements [A.Num 1, A.Num 2, A.Bol True]

-- distinct binder names (duplicates are rejected by the type checker)
genDistinct :: Int -> Gen [Name]
genDistinct n = do
  ts <- take n . nub <$> shuffle pool
  mapM (\t -> Name t <$> genRange) ts

genPattern :: [Name] -> Gen A.Pattern
genPattern [] = oneof [A.PattLit <$> genLit, pure (A.PattWildcard (mkRange (mkPos 1 1) (mkPos 1 2)))]
genPattern [x] = oneof [pure (A.PattBinder x), pure (A.PattTuple [A.PattBinder x])]
genPattern (x : xs) =
  oneof
    [ A.PattTuple . (A.PattBinder x :) . pure <$> genPattern xs,
      A.PattConstructor (Name "Just" Nothing) . (A.PattBinder x :) . pure <$> genPattern xs
    ]

genExpr :: Int -> Gen T.Expr
genExpr 0 =
  oneof
    [ T.Lit <$> genLit <*> genType <*> genRange,
      T.Var <$> genName <*> genType <*> genRange,
      T.Const <$> genName <*> genType <*> genRange,
      pure (T.Op (ArithOp (Add Nothing)) tyInt)
    ]
genExpr n =
  oneof
    [ genExpr 0,
      T.App <$> sub <*> sub <*> genRange,
      T.Lam <$> genName <*> genType <*> sub <*> genRange,
      T.Tuple <$> vectorOf 2 sub,
      T.OutT <$> elements [0, 1] <*> sub,
      do
        k <- elements [1, 2]
        bs <- genDistinct k
        ts <- vectorOf (length bs) genType
        T.Quant <$> sub <*> pure (zip bs ts) <*> sub <*> sub <*> genRange,
      T.ArrIdx <$> sub <*> sub <*> genRange,
      T.ArrUpd <$> sub <*> sub <*> sub <*> genRange,
      do
        k <- elements [1, 2]
        clauses <- vectorOf k genClause
        T.Case <$> sub <*> pure clauses <*> genRange,
      do
        k <- elements [1, 2]
        ds <- genDistinct k
        es <- vectorOf (length ds) sub
        T.Subst <$> sub <*> pure (zip ds es),
      T.Chain <$> genChain
    ]
  where
    sub = genExpr (n - 1)
    genClause = do
      k <- elements [0, 1, 2]
      bs <- genDistinct k
      T.CaseClause <$> genPattern bs <*> sub
    genChain =
      oneof
        [ T.Pure <$> sub,
          T.More <$> (T.Pure <$> sub) <*> pure (ArithOp (Add Nothing)) <*> genType <*> sub
        ]

genSubs :: Gen [(Text, T.Expr)]
genSubs = do
  k <- elements [0, 1, 2, 3]
  ks <- take k . nub <$> shuffle pool
  mapM (\t -> (,) t <$> genExpr 2) ks

-------------------------------------------------------------------------------
-- Reference: freshen every binder to a globally unique name that cannot clash
-- with anything in the pool, then substitute blindly.  Capture is impossible.
-------------------------------------------------------------------------------

type Ren = Map.Map Text Text

newName :: State Int Text
newName = do n <- get; put (n + 1); pure (Text.pack ("#" ++ show n))

renN :: Ren -> Name -> Name
renN m (Name t r) = Name (fromMaybe t (Map.lookup t m)) r

bindN :: Ren -> [Name] -> State Int (Ren, [Name])
bindN m [] = pure (m, [])
bindN m (Name t r : xs) = do
  t' <- newName
  (m', xs') <- bindN (Map.insert t t' m) xs
  pure (m', Name t' r : xs')

renP :: Ren -> A.Pattern -> A.Pattern
renP _ p@A.PattLit {} = p
renP m (A.PattBinder x) = A.PattBinder (renN m x)
renP _ p@A.PattWildcard {} = p
renP m (A.PattTuple ps) = A.PattTuple (map (renP m) ps)
renP m (A.PattConstructor c ps) = A.PattConstructor c (map (renP m) ps)

freshen :: Ren -> T.Expr -> State Int T.Expr
freshen _ e@T.Lit {} = pure e
freshen m (T.Var x t l) = pure (T.Var (renN m x) t l)
freshen m (T.Const x t l) = pure (T.Const (renN m x) t l)
freshen _ e@T.Op {} = pure e
freshen m (T.Chain c) = T.Chain <$> freshenChain m c
freshen m (T.App f a l) = T.App <$> freshen m f <*> freshen m a <*> pure l
freshen m (T.Lam (Name x r) t b l) = do
  x' <- newName
  T.Lam (Name x' r) t <$> freshen (Map.insert x x' m) b <*> pure l
freshen m (T.Tuple es) = T.Tuple <$> mapM (freshen m) es
freshen m (T.OutT i e) = T.OutT i <$> freshen m e
freshen m (T.Quant op bs r b l) = do
  op' <- freshen m op
  (m', xs') <- bindN m (map fst bs)
  T.Quant op' (zip xs' (map snd bs)) <$> freshen m' r <*> freshen m' b <*> pure l
freshen m (T.ArrIdx a i l) = T.ArrIdx <$> freshen m a <*> freshen m i <*> pure l
freshen m (T.ArrUpd a i v l) = T.ArrUpd <$> freshen m a <*> freshen m i <*> freshen m v <*> pure l
freshen m (T.Case s cs l) = T.Case <$> freshen m s <*> mapM clause cs <*> pure l
  where
    clause (T.CaseClause p body) = do
      (m', _) <- bindN m (A.extractBinder p)
      T.CaseClause (renP m' p) <$> freshen m' body
freshen m (T.Subst b tbl) = do
  (m', ds') <- bindN m (map fst tbl)
  rng <- mapM (freshen m . snd) tbl
  T.Subst <$> freshen m' b <*> pure (zip ds' rng)
freshen _ e@T.EHole {} = pure e

freshenChain :: Ren -> T.Chain -> State Int T.Chain
freshenChain m (T.Pure e) = T.Pure <$> freshen m e
freshenChain m (T.More c o t e) = T.More <$> freshenChain m c <*> pure o <*> pure t <*> freshen m e

-- After freshening, every binder in the target is "#n", while every
-- substitution key and every free substitutable name in a replacement comes
-- from the pool.  So no target binder can shadow a key or capture a replacement
-- name: blind replacement is capture-avoiding and shadow-correct.
naive :: [(Text, T.Expr)] -> T.Expr -> T.Expr
naive _ e@T.Lit {} = e
naive sb e@(T.Var x _ _) = fromMaybe e (lookup (nameToText x) sb)
naive sb e@(T.Const x _ _) = fromMaybe e (lookup (nameToText x) sb)
naive _ e@T.Op {} = e
naive sb (T.Chain c) = T.Chain (naiveChain sb c)
naive sb (T.App f a l) = T.App (naive sb f) (naive sb a) l
naive sb (T.Lam x t b l) = T.Lam x t (naive sb b) l
naive sb (T.Tuple es) = T.Tuple (map (naive sb) es)
naive sb (T.OutT i e) = T.OutT i (naive sb e)
naive sb (T.Quant op bs r b l) = T.Quant (naive sb op) bs (naive sb r) (naive sb b) l
naive sb (T.ArrIdx a i l) = T.ArrIdx (naive sb a) (naive sb i) l
naive sb (T.ArrUpd a i v l) = T.ArrUpd (naive sb a) (naive sb i) (naive sb v) l
naive sb (T.Case s cs l) = T.Case (naive sb s) (map cl cs) l
  where
    cl (T.CaseClause p body) = T.CaseClause p (naive sb body)
naive sb (T.Subst b tbl) = T.Subst (naive sb b) [(x, naive sb e) | (x, e) <- tbl]
naive _ e@T.EHole {} = e

naiveChain :: [(Text, T.Expr)] -> T.Chain -> T.Chain
naiveChain sb (T.Pure e) = T.Pure (naive sb e)
naiveChain sb (T.More c o t e) = T.More (naiveChain sb c) o t (naive sb e)

-------------------------------------------------------------------------------
-- Alpha equivalence.  Compares constructors, binding structure, node ranges
-- and every 'Name' range -- the last one matters because 'Eq Name' ignores
-- ranges on purpose ("compare regardless of their locations").
--
-- Types are compared with 'Eq Type', which ignores the ranges inside a type
-- and, through 'Eq Name', the ranges of names within it; genType emits only
-- Int and Bool anyway.  So type metadata is out of reach here: the current
-- Subst2 passes each 'Type' through unchanged and never constructs one, and
-- these properties do not guard that it stays that way.
-------------------------------------------------------------------------------

alphaEq :: T.Expr -> T.Expr -> Bool
alphaEq = go 0 Map.empty Map.empty
  where
    nm d c1 c2 (Name t1 r1) (Name t2 r2) =
      r1 == r2 && case (Map.lookup t1 c1, Map.lookup t2 c2) of
        (Just i, Just j) -> i == j
        (Nothing, Nothing) -> t1 == t2
        _ -> False
      where
        _ = d
    ext d c xs = (d + length xs, foldr (\(i, Name t _) acc -> Map.insert t i acc) c (zip [d ..] xs))
    go d c1 c2 e1 e2 = case (e1, e2) of
      (T.Lit a t l, T.Lit a' t' l') -> a == a' && t == t' && l == l'
      (T.Var x t l, T.Var x' t' l') -> nm d c1 c2 x x' && t == t' && l == l'
      (T.Const x t l, T.Const x' t' l') -> nm d c1 c2 x x' && t == t' && l == l'
      (T.Op o t, T.Op o' t') -> o == o' && t == t'
      (T.Chain a, T.Chain a') -> chain d c1 c2 a a'
      (T.App f a l, T.App f' a' l') -> go d c1 c2 f f' && go d c1 c2 a a' && l == l'
      (T.Lam x t b l, T.Lam x' t' b' l') ->
        t == t'
          && l == l'
          && rangeEq x x'
          && let (d1, c1') = ext d c1 [x]; (_, c2') = ext d c2 [x'] in go d1 c1' c2' b b'
      (T.Tuple es, T.Tuple es') -> length es == length es' && and (zipWith (go d c1 c2) es es')
      (T.OutT i a, T.OutT i' a') -> i == i' && go d c1 c2 a a'
      (T.Quant op bs r b l, T.Quant op' bs' r' b' l') ->
        go d c1 c2 op op'
          && map snd bs == map snd bs'
          && l == l'
          && length bs == length bs'
          && and (zipWith rangeEq (map fst bs) (map fst bs'))
          && let (d1, c1') = ext d c1 (map fst bs); (_, c2') = ext d c2 (map fst bs')
              in go d1 c1' c2' r r' && go d1 c1' c2' b b'
      (T.ArrIdx a i l, T.ArrIdx a' i' l') -> go d c1 c2 a a' && go d c1 c2 i i' && l == l'
      (T.ArrUpd a i v l, T.ArrUpd a' i' v' l') ->
        go d c1 c2 a a' && go d c1 c2 i i' && go d c1 c2 v v' && l == l'
      (T.Case s cs l, T.Case s' cs' l') ->
        go d c1 c2 s s' && l == l' && length cs == length cs' && and (zipWith clause cs cs')
        where
          clause (T.CaseClause p body) (T.CaseClause p' body') =
            shapeEq p p'
              && let bs = A.extractBinder p
                     bs' = A.extractBinder p'
                     (d1, c1') = ext d c1 bs
                     (_, c2') = ext d c2 bs'
                  in length bs == length bs'
                       && and (zipWith rangeEq bs bs')
                       && go d1 c1' c2' body body'
      (T.Subst b tbl, T.Subst b' tbl') ->
        length tbl == length tbl'
          && and (zipWith rangeEq (map fst tbl) (map fst tbl'))
          && and (zipWith (go d c1 c2) (map snd tbl) (map snd tbl'))
          && let (d1, c1') = ext d c1 (map fst tbl); (_, c2') = ext d c2 (map fst tbl')
              in go d1 c1' c2' b b'
      (T.EHole h, T.EHole h') -> h == h'
      _ -> False
    chain d c1 c2 (T.Pure e) (T.Pure e') = go d c1 c2 e e'
    chain d c1 c2 (T.More a o t e) (T.More a' o' t' e') =
      chain d c1 c2 a a' && o == o' && t == t' && go d c1 c2 e e'
    chain _ _ _ _ _ = False
    rangeEq (Name _ r) (Name _ r') = r == r'
    -- pattern shape, ignoring binder names
    shapeEq (A.PattLit a) (A.PattLit b) = a == b
    shapeEq (A.PattBinder _) (A.PattBinder _) = True
    shapeEq (A.PattWildcard a) (A.PattWildcard b) = a == b
    shapeEq (A.PattTuple a) (A.PattTuple b) = length a == length b && and (zipWith shapeEq a b)
    shapeEq (A.PattConstructor c a) (A.PattConstructor c' b) =
      c == c' && rangeEq c c' && length a == length b && and (zipWith shapeEq a b)
    shapeEq _ _ = False

-------------------------------------------------------------------------------

reference :: [(Text, T.Expr)] -> T.Expr -> T.Expr
reference sb e = naive sb (evalState (freshen Map.empty e) 0)

runState' :: [(Text, T.Expr)] -> T.Expr -> T.Expr
runState' sb e = evalState (substExpr sb e) (0 :: Int)

runWP' :: [[Text]] -> [(Text, T.Expr)] -> T.Expr -> T.Expr
runWP' scopes sb e = case runWP (substExpr sb e) (Map.empty, scopes) 0 of
  Right (r, _, _) -> r
  Left err -> error (show err)

prepareState' :: [(Text, T.Expr)] -> T.Expr -> T.Expr
prepareState' sb e = evalState (renameForSubstitution sb e) (0 :: Int)

prepareWP' :: [[Text]] -> [(Text, T.Expr)] -> T.Expr -> T.Expr
prepareWP' scopes sb e =
  case runWP (renameForSubstitution sb e) (Map.empty, scopes) 0 of
    Right (r, _, _) -> r
    Left err -> error (show err)

prop_alpha :: ([(Text, T.Expr)] -> T.Expr -> T.Expr) -> Property
prop_alpha runner = forAll ((,) <$> genSubs <*> genExpr 7) $ \(sb, e) ->
  let actual = runner sb e
      expect = reference sb e
   in counterexample (unlines ["input:  " ++ show e, "subs:   " ++ show sb, "actual: " ++ show actual, "expect: " ++ show expect]) $
        alphaEq actual expect

prop_fv :: ([(Text, T.Expr)] -> T.Expr -> T.Expr) -> Property
prop_fv runner = forAll ((,) <$> genSubs <*> genExpr 7) $ \(sb, e) ->
  let fv = freeVarsT e
      live = [(x, ex) | (x, ex) <- sb, x `Set.member` fv]
      expect = (fv Set.\\ Set.fromList (map fst live)) <> foldMap (freeVarsT . snd) live
      actual = freeVarsT (runner sb e)
   in counterexample (unlines ["input: " ++ show e, "subs:  " ++ show sb, "actual fv: " ++ show actual, "expect fv: " ++ show expect]) $
        actual == expect

prop_prepareAlpha :: ([(Text, T.Expr)] -> T.Expr -> T.Expr) -> Property
prop_prepareAlpha runner = forAll ((,) <$> genSubs <*> genExpr 7) $ \(sb, e) ->
  let actual = runner sb e
   in counterexample (unlines ["input:  " ++ show e, "subs:   " ++ show sb, "prepared: " ++ show actual]) $
        alphaEq actual e

prop_idem :: ([(Text, T.Expr)] -> T.Expr -> T.Expr) -> Property
prop_idem runner = forAll ((,) <$> genSubs <*> genExpr 7) $ \(sb, e) ->
  let prepared = runner sb e
      preparedAgain = runner sb prepared
   in counterexample (unlines ["input: " ++ show e, "subs:  " ++ show sb, "once:  " ++ show prepared, "twice: " ++ show preparedAgain]) $
        preparedAgain == prepared

prop_noop :: Property
prop_noop = forAll (genExpr 7) $ \e -> runState' [] e == e

-------------------------------------------------------------------------------
-- Half the runs below rest on alphaEq -- prop_alpha, prop_prepareAlpha and
-- prop_freshenAlpha, 6 of 12 -- and nothing else checks it: a comparator that
-- is too lenient would make those pass blindly.  (prop_fv, prop_idem and
-- prop_noop use set or exact equality instead.)  So alphaEq gets hand-written
-- cases, negatives included.  Note that 'Eq Name' ignores
-- ranges on purpose ("compare regardless of their locations"), which is why
-- alphaEq carries its own rangeEq and why these cases pin it down.
-------------------------------------------------------------------------------

ra, rb :: Maybe Range
ra = Just (mkRange (mkPos 1 1) (mkPos 1 2))
rb = Just (mkRange (mkPos 2 3) (mkPos 2 9))

nmR :: Text -> Maybe Range -> Name
nmR = Name

vr :: Text -> Maybe Range -> T.Expr
vr t r = T.Var (nmR t r) tyInt r

lamE :: Text -> Maybe Range -> T.Expr -> T.Expr
lamE x r b = T.Lam (nmR x r) tyInt b ra

quantE :: Text -> Maybe Range -> T.Expr -> T.Expr
quantE x r b = T.Quant (vr "op" ra) [(nmR x r, tyInt)] b b ra

caseE :: Text -> Maybe Range -> T.Expr -> T.Expr
caseE x r b = T.Case (vr "s" ra) [T.CaseClause (A.PattBinder (nmR x r)) b] ra

ctorE :: Maybe Range -> T.Expr
ctorE r =
  T.Case
    (vr "s" ra)
    [T.CaseClause (A.PattConstructor (nmR "Just" r) [A.PattBinder (nmR "x" ra)]) (vr "x" ra)]
    ra

substE :: Text -> Maybe Range -> T.Expr -> T.Expr
substE x r b = T.Subst b [(nmR x r, vr "e" ra)]

-- These three put the same text on a binder and on a position that the binder
-- does NOT scope over, so the outside occurrence stays free.  Renaming the
-- binder must therefore make the two trees differ: it pins the scope boundary
-- rather than the range comparisons.
quantOpE :: Text -> T.Expr
quantOpE t = T.Quant (vr t ra) [(nmR t ra, tyInt)] (vr t ra) (vr t ra) ra

caseScrutE :: Text -> T.Expr
caseScrutE t = T.Case (vr t ra) [T.CaseClause (A.PattBinder (nmR t ra)) (vr t ra)] ra

substValE :: Text -> T.Expr
substValE t = T.Subst (vr t ra) [(nmR t ra, vr t ra)]

alphaEqCases :: [(String, Bool, T.Expr, T.Expr)]
alphaEqCases =
  [ ("lam: renamed binder", True, lamE "x" ra (vr "x" ra), lamE "y" ra (vr "y" ra)),
    ("lam: free name differs", False, lamE "x" ra (vr "y" ra), lamE "x" ra (vr "z" ra)),
    ("lam: bound vs free", False, lamE "x" ra (vr "x" ra), lamE "y" ra (vr "x" ra)),
    ("lam: binder range differs", False, lamE "x" ra (vr "x" ra), lamE "x" rb (vr "x" ra)),
    ("lam: occurrence range differs", False, lamE "x" ra (vr "x" ra), lamE "x" ra (vr "x" rb)),
    ( "lam: binder type differs",
      False,
      T.Lam (nmR "x" ra) tyInt (vr "x" ra) ra,
      T.Lam (nmR "x" ra) tyBool (vr "x" ra) ra
    ),
    ( "lam: node range differs",
      False,
      T.Lam (nmR "x" ra) tyInt (vr "x" ra) ra,
      T.Lam (nmR "x" ra) tyInt (vr "x" ra) rb
    ),
    ("quant: renamed binder", True, quantE "x" ra (vr "x" ra), quantE "y" ra (vr "y" ra)),
    ("quant: binder range differs", False, quantE "x" ra (vr "x" ra), quantE "x" rb (vr "x" ra)),
    ("case: renamed pattern binder", True, caseE "x" ra (vr "x" ra), caseE "y" ra (vr "y" ra)),
    ("case: pattern binder range differs", False, caseE "x" ra (vr "x" ra), caseE "x" rb (vr "x" ra)),
    ("case: constructor name range differs", False, ctorE ra, ctorE rb),
    ("case: same constructor, same ranges", True, ctorE ra, ctorE ra),
    ("subst: renamed domain", True, substE "x" ra (vr "x" ra), substE "y" ra (vr "y" ra)),
    ("quant: operator is outside the binder scope", False, quantOpE "x", quantOpE "y"),
    ("case: scrutinee is outside the pattern scope", False, caseScrutE "x", caseScrutE "y"),
    ("subst: table values are outside the domain scope", False, substValE "x", substValE "y"),
    ("subst: domain range differs", False, substE "x" ra (vr "x" ra), substE "x" rb (vr "x" ra))
  ]

alphaEqTests :: TestTree
alphaEqTests =
  testGroup
    "alphaEq"
    [ testCase name (alphaEq a b @?= expected)
    | (name, expected, a, b) <- alphaEqCases
    ]

prop_freshenAlpha :: Property
prop_freshenAlpha = forAll (genExpr 7) $ \e ->
  alphaEq e (evalState (freshen Map.empty e) 0)

-- Keep routine runs meaningful while allowing the command line to request more.
minimumCases :: QuickCheckTests -> QuickCheckTests
minimumCases (QuickCheckTests n) = QuickCheckTests (max 1000 n)

tests :: TestTree
tests =
  adjustOption minimumCases $
    testGroup
      "Subst2 properties"
      [ alphaEqTests,
        -- The oracle reserves the "#" prefix for globally fresh binders, so
        -- keep generated names outside that namespace; otherwise 'freshen'
        -- could collide with a generated name and cease to be a sound
        -- reference implementation.  Reserving the whole prefix is stricter
        -- than the bare requirement (today 'newName' only makes "#0", "#1",
        -- ...) and stays correct if that format ever changes.
        testCase "the name pool is disjoint from freshened binders" $
          filter (Text.isPrefixOf (Text.pack "#")) pool @?= [],
        testProperty "freshen preserves alpha-equivalence" prop_freshenAlpha,
        testProperty "prepare is alpha-equivalent to input (State Int)" (prop_prepareAlpha prepareState'),
        testProperty "prepare is alpha-equivalent to input (WP, scope [a,b,c])" (prop_prepareAlpha (prepareWP' [["a", "b", "c"]])),
        testProperty "prepare is idempotent (State Int)" (prop_idem prepareState'),
        testProperty "prepare is idempotent (WP, scope [a,b,c])" (prop_idem (prepareWP' [["a", "b", "c"]])),
        testProperty "identity (empty substitution)" prop_noop,
        testProperty "free-variable law (State Int)" (prop_fv runState'),
        testProperty "free-variable law (WP, empty scope)" (prop_fv (runWP' [])),
        testProperty "free-variable law (WP, scope [a,b,c])" (prop_fv (runWP' [["a", "b", "c"]])),
        testProperty "alpha-equivalence vs reference (State Int)" (prop_alpha runState'),
        testProperty "alpha-equivalence vs reference (WP, empty scope)" (prop_alpha (runWP' [])),
        testProperty "alpha-equivalence vs reference (WP, scope [a,b,c])" (prop_alpha (runWP' [["a", "b", "c"]]))
      ]
