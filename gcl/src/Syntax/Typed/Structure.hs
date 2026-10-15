-- | Shared typed-expression structure and source-preserving name utilities.
module Syntax.Typed.Structure
  ( RedexPath,
    RTree (..),
    RT,
    pathChildren,
    childBinders,
    withPathChildren,
    replaceAt,
    allNames,
    renameName,
    renamePatternBinders,
  )
where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Syntax.Abstract.Types (Pattern (..), extractBinder)
import Syntax.Common.Types (Name (..), nameToText)
import Syntax.Typed.Types

type RedexPath = [Int]

data RTree a = Node a [RTree a] deriving (Eq, Show)

type RT = RTree Bool

type Renaming = [(Text, Text)]

-- | The subexpressions into which a reduction path may descend, in path-index
-- order.  This is the single source of truth for path indexing in this module.
pathChildren :: Expr -> [Expr]
pathChildren (Lit _ _ _) = []
pathChildren (Var _ _ _) = []
pathChildren (Const _ _ _) = []
pathChildren (Op _ _) = []
pathChildren (Chain chain) = chainExprs chain
pathChildren (App function argument _) = [function, argument]
pathChildren (Lam _ _ body _) = [body]
pathChildren (Tuple expressions) = expressions
pathChildren (OutT _ expression) = [expression]
pathChildren (Quant operator _ range body _) = [operator, range, body]
pathChildren (ArrIdx array index _) = [array, index]
pathChildren (ArrUpd array index value _) = [array, index, value]
pathChildren (Case scrutinee clauses _) = scrutinee : map clauseBody clauses
  where
    clauseBody (CaseClause _ body) = body
pathChildren (Subst subject assignments) = subject : map snd assignments
pathChildren (EHole _) = []

-- | Binders whose scope covers each corresponding path child.
childBinders :: Expr -> [[Name]]
childBinders (Lam binder _ _ _) = [[binder]]
childBinders (Quant _ binders _ _ _) = [[], names, names]
  where
    names = map fst binders
childBinders (Case _ clauses _) = [] : map clauseBinders clauses
  where
    clauseBinders (CaseClause pattern' _) = extractBinder pattern'
childBinders (Subst _ assignments) =
  map fst assignments : map (const []) assignments
childBinders expression = map (const []) (pathChildren expression)

-- | Rebuild an expression with its path children replaced.  The input list
-- must have the same shape and order as 'pathChildren'.
withPathChildren :: Expr -> [Expr] -> Expr
withPathChildren expression@(Lit _ _ _) [] = expression
withPathChildren expression@(Var _ _ _) [] = expression
withPathChildren expression@(Const _ _ _) [] = expression
withPathChildren expression@(Op _ _) [] = expression
withPathChildren expression@(EHole _) [] = expression
withPathChildren (Chain chain) children
  | length children == length (chainExprs chain) =
      Chain (withChainExprs chain children)
withPathChildren (App _ _ range) [function, argument] =
  App function argument range
withPathChildren (Lam binder t _ range) [body] =
  Lam binder t body range
withPathChildren (Tuple expressions) children
  | length children == length expressions = Tuple children
withPathChildren (OutT index _) [expression] = OutT index expression
withPathChildren (Quant _ binders _ _ range) [operator, restriction, body] =
  Quant operator binders restriction body range
withPathChildren (ArrIdx _ _ range) [array, index] =
  ArrIdx array index range
withPathChildren (ArrUpd _ _ _ range) [array, index, value] =
  ArrUpd array index value range
withPathChildren (Case _ clauses range) (scrutinee : bodies)
  | length bodies == length clauses =
      Case scrutinee (zipWith withBody clauses bodies) range
  where
    withBody (CaseClause pattern' _) body = CaseClause pattern' body
withPathChildren (Subst _ assignments) (subject : values)
  | length values == length assignments =
      Subst subject (zip (map fst assignments) values)
withPathChildren expression children =
  error $
    "withPathChildren: expected "
      <> show (length (pathChildren expression))
      <> " children, given "
      <> show (length children)

-- Chain paths are rightmost first, matching the existing renderer.
chainExprs :: Chain -> [Expr]
chainExprs (Pure expression) = [expression]
chainExprs (More chain _ _ expression) = expression : chainExprs chain

withChainExprs :: Chain -> [Expr] -> Chain
withChainExprs chain expressions
  | length expressions == length (chainExprs chain) = go chain expressions
  | otherwise =
      error $
        "withChainExprs: expected "
          <> show (length (chainExprs chain))
          <> " operands, given "
          <> show (length expressions)
  where
    go (Pure _) [expression] = Pure expression
    go (More chain' operator t _) (expression : rest) =
      More (go chain' rest) operator t expression
    go _ _ = error "withChainExprs: chainExprs and rebuild disagree"

replaceAt :: Int -> a -> [a] -> Maybe [a]
replaceAt index _ _ | index < 0 = Nothing
replaceAt _ _ [] = Nothing
replaceAt 0 replacement (_ : rest) = Just (replacement : rest)
replaceAt index replacement (value : rest) =
  (value :) <$> replaceAt (index - 1) replacement rest

renameName :: Renaming -> Name -> Name
renameName renaming name@(Name text range) =
  case lookup text renaming of
    Nothing -> name
    Just text' -> Name text' range

-- | Rewrite the binders a pattern introduces. 'renameFree' leaves every binder
--   alone, so the caller renames them here.
renamePatternBinders :: Renaming -> Pattern -> Pattern
renamePatternBinders _ pattern'@PattLit {} = pattern'
renamePatternBinders renaming (PattBinder x) = PattBinder (renameName renaming x)
renamePatternBinders _ pattern'@PattWildcard {} = pattern'
renamePatternBinders renaming (PattTuple patterns) =
  PattTuple (map (renamePatternBinders renaming) patterns)
-- A constructor name is not a binder.
renamePatternBinders renaming (PattConstructor constructor patterns) =
  PattConstructor constructor (map (renamePatternBinders renaming) patterns)

-- | Term names and binder names in a region. Free names alone are insufficient
--   when a proposed target could collide with an inner binder.
allNames :: Expr -> Set Text
allNames Lit {} = mempty
allNames (Var x _ _) = Set.singleton (nameToText x)
allNames (Const x _ _) = Set.singleton (nameToText x)
allNames Op {} = mempty
allNames (Chain chain) = allNamesChain chain
allNames (App function argument _) = allNames function <> allNames argument
allNames (Lam x _ body _) = Set.insert (nameToText x) (allNames body)
allNames (Tuple elements) = foldMap allNames elements
allNames (OutT _ e) = allNames e
allNames (Quant operator binders range body _) =
  allNames operator
    <> Set.fromList (map (nameToText . fst) binders)
    <> allNames range
    <> allNames body
allNames (ArrIdx array index _) = allNames array <> allNames index
allNames (ArrUpd array index value _) =
  allNames array <> allNames index <> allNames value
allNames (Case scrutinee clauses _) =
  allNames scrutinee <> foldMap allNamesClause clauses
allNames (Subst body table) =
  allNames body
    <> Set.fromList (map (nameToText . fst) table)
    <> foldMap (allNames . snd) table
allNames EHole {} = mempty

allNamesChain :: Chain -> Set Text
allNamesChain (Pure e) = allNames e
allNamesChain (More chain _ _ e) = allNamesChain chain <> allNames e

allNamesClause :: CaseClause -> Set Text
allNamesClause (CaseClause pattern' body) =
  Set.fromList (map nameToText (extractBinder pattern')) <> allNames body
