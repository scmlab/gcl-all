{-# LANGUAGE OverloadedStrings #-}

module Syntax.AbstractLN.Scope where

import Control.Arrow (first, second, (***))
import Data.Bifunctor (bimap)
import Data.Text (Text, pack, append)
import Syntax.AbstractLN.Types
import Syntax.Common (Name (..), nameToText)
import Documentation.SBV.Examples.ProofTools.BMC (S(x))

instance LocallyNameless Var where
  close xis v@(FName x) = maybe v BName (lookup x xis)
  close _ b@BName {} = b

  open _ f@FName {} = f
  open ixs v@(BName i) = maybe v FName (lookup i ixs)

instance LocallyNameless Type where
  close _ lit@TLit{} = lit
  close xis (TVar v l) = TVar (close xis v) l
  close xis (TApp t0 t1 l) = TApp (close xis t0) (close xis t1) l
  close xis (TForall xs t l) = TForall xs (close xis' t) l
    where xis' = map (second (length xs +)) xis
  close _ TType = TType

  open _ lit@TLit{} = lit
  open ixs (TVar v l) = TVar (open ixs v) l
  open ixs (TApp t0 t1 l) = TApp (open ixs t0) (open ixs t1) l
  open ixs (TForall xs t l) = TForall xs (open ixs' t) l
     where ixs' = map (first (length xs +)) ixs
  open _ TType = TType

instance LocallyNameless Expr where

  close _ lit@Lit{} = lit
  close xis (Var v l) = Var (close xis v) l
  close xis (Chain ch) = Chain (close xis ch)
  close xis (App e0 e1 l) = App (close xis e0) (close xis e1) l
  close xis (Lam x e l) = Lam x (close xis' e) l
     where xis' = map (second (1+)) xis
  close xis (Quant op xs ran body l) =
     Quant (close xis op) xs (close xis' ran) (close xis' body) l
    where xis' = map (second (length xs +)) xis
  close xis (Case m cls l) =
     Case (close xis m) (map (closeCl xis) cls) l
    where closeCl xis (ptn, e) = (ptn, close xis' e)
            where xs = extractBinders ptn
                  xis' = map (second (length xs +)) xis
  close _ h@EHole{} = h

  open _ lit@Lit{} = lit
  open ixs (Var v l) = Var (open ixs v) l
  open ixs (Chain ch) = Chain (open ixs ch)
  open ixs (App e0 e1 l) = App (open ixs e0) (open ixs e1) l
  open ixs (Lam x e l) = Lam x (open ixs' e) l
     where ixs' = map (first (1+)) ixs
  open ixs (Quant op xs ran body l) =
     Quant (open ixs op) xs (open ixs' ran) (open ixs' body) l
    where ixs' = map (first (length xs +)) ixs
  open ixs (Case m cls l) =
     Case (open ixs m) (map (openCl ixs) cls) l
    where openCl ixs (ptn, e) = (ptn, open ixs' e)
            where xs = extractBinders ptn
                  ixs' = map (first (length xs +)) ixs
  open _ h@EHole{} = h

instance LocallyNameless Chain where

  close xis (Pure e l) = Pure (close xis e) l
  close xis (More ch op e l) = More (close xis ch) op (close xis e) l

  open ixs (Pure e l) = Pure (open ixs e) l
  open ixs (More ch op e l) = More (open ixs ch) op (open ixs e) l

instance LocallyNameless Stmt where

  close _ s@Skip {} = s
  close _ s@Abort {} = s
  close xis (Assign asgns l) =
     Assign (map (bimap (close xis) id *** close xis) asgns) l
  close xis (AAssign arr idx val l) =
     AAssign (close xis arr) (close xis idx) (close xis val) l
  close xis (Assert e l) =
     Assert (close xis e) l
  close xis (LoopInvariant inv bnd l) =
     LoopInvariant (close xis inv) (close xis bnd) l
  close xis (Do gcmds l) =
     Do (map (close xis) gcmds) l
  close xis (If gcmds l) =
     If (map (close xis) gcmds) l
  close _ s@Spec{} = s
  close xis (Alloc v es l) =
     Alloc (close xis v) (map (close xis) es) l
  close xis (HLookup v e l) =
     HLookup (close xis v) (close xis e) l
  close xis (HMutate a v l) =
     HMutate (close xis a) (close xis v) l
  close xis (Dispose a l) =
     Dispose (close xis a) l

  open _ s@Skip {} = s
  open _ s@Abort {} = s
  open ixs (Assign asgns l) =
     Assign (map (bimap (open ixs) id *** open ixs) asgns) l
  open ixs (AAssign arr idx val l) =
     AAssign (open ixs arr) (open ixs idx) (open ixs val) l
  open ixs (Assert e l) =
     Assert (open ixs e) l
  open ixs (LoopInvariant inv bnd l) =
     LoopInvariant (open ixs inv) (open ixs bnd) l
  open ixs (Do gcmds l) =
     Do (map (open ixs) gcmds) l
  open ixs (If gcmds l) =
     If (map (open ixs) gcmds) l
  open _ s@Spec{} = s
  open ixs (Alloc v es l) =
     Alloc (open ixs v) (map (open ixs) es) l
  open ixs (HLookup v e l) =
     HLookup (open ixs v) (open ixs e) l
  open ixs (HMutate a v l) =
     HMutate (open ixs a) (open ixs v) l
  open ixs (Dispose a l) =
     Dispose (open ixs a) l

instance LocallyNameless GdCmd where
  close xis (GdCmd g stmts l) =
     GdCmd (close xis g) (close xis stmts) l
  open ixs (GdCmd g stmts l) =
     GdCmd (open ixs g) (open ixs stmts) l

instance LocallyNameless a => LocallyNameless [a] where
  close xis = map (close xis)
  open ixs = map (open ixs)

----------------------------------------------------
-- | Opening various constructs

-- quantification

openQuant :: [Text] -> Expr -> (Expr, [Text], Expr, Expr)
openQuant zs (Quant op xs ran body _) =
   (op, xs, open ixs ran, open ixs body)
  where ixs = zip [0..] (freshIds zs (reverse xs))
openQuant _ _ = error "open: not a Quant"

quant :: Expr -> [Text] -> Expr -> Expr -> Expr
quant op xs ran body =
    Quant op xs (close xis ran) (close xis body) Nothing
  where xis = zip (reverse xs) [0..]

-- lambda

openLam :: [Text] -> Expr -> (Text, Expr)
openLam zs (Lam x e _) = (x', open [(0,x')] e)
   where x' = fresh zs x
openLam _ _ = error "open: not a Lam"

lam :: Text -> Expr -> Expr
lam x e = Lam x (close [(x,0)] e) Nothing

openLams :: [Text] -> Expr -> ([Text], Expr)
openLams zs e =
     (xs', open (zip [0..] (reverse xs')) e')
    where (xs, e') = unwindLam e
          xs' = freshIds zs xs

unwindLam :: Expr -> ([Text], Expr)
unwindLam (Lam x e _) = first (x:) (unwindLam e)
unwindLam e = ([],e)

lams :: [Text] -> Expr -> Expr
lams [] e = e
lams (x:xs) e = Lam x (close [(x,0)] (lams xs e)) Nothing

-- Forall in Type

openTForall :: [Text] -> Type -> ([Text], Type)
openTForall zs (TForall xs t _) =
    (xs', open (zip [0..] (reverse xs')) t)
  where xs' = freshIds zs xs
openTForall _ _ = error "open: not a Forall"

tforall :: [Text] -> Type -> Type
tforall xs t =
  TForall xs (close (zip (reverse xs) [0..]) t) Nothing

-- case branch

openCaseBranch :: [Text] -> (Pattern, Expr) -> ([Text], Expr)
openCaseBranch zs (p, e) =
   (xs', open (zip [0..] xs') e)
  where xs = extractBinders p
        xs' = freshIds zs xs

caseBranch :: [Text] -> Pattern -> Expr -> (Pattern, Expr)
caseBranch xs p e =
  (renameBinders xs p, close (zip xs [0..]) e)

-- program

openProgram :: [Text] -> Program ->
   ([(Name, Int)],   -- user-defined types. unchanged
    [(Name, Type)],  -- data constructors. unchanged
    [(Name, Maybe Type, Maybe Expr)],  -- vals. opened.
    [(Name, Type)],  -- GCL constants. binders, possibly renamed
    [(Name, Type)],  -- GCL constants. binders, possibly renamed
    [Expr],   -- global properties, opened
    [Stmt],   -- program, opened
    [Pragma]  -- pragmas. unchanged.
   )
openProgram zs
    (Program types constrs vals
         consts vars props stmts pragma) =
    ( types, constrs,
      map (third (fmap (open boundvars'))) vals,
      consts', vars',
      map (open boundvars') props,
      map (open boundvars') stmts,
      pragma)
  where
   cntrNames = map (nameToText . fst) constrs
   valNames = map (nameToText . fst3) vals
      where fst3 (x,_,_) = x

   zs' = cntrNames ++ valNames ++ zs
   boundvars = freshIds zs' (map (nameToText . fst)
                    (consts ++ vars))
   boundvars' = zip [0..] (reverse boundvars)
   (consts', vars') =
      splitAt (length consts) $
       zipWith rename (consts ++ vars) boundvars
     where rename (Name _ l, t) x = (Name x l, t)

third f (x,y,z) = (x,y,f z)

program :: [(Name, Int)]   -- user-defined types. unchanged
        -> [(Name, Type)]  -- data constructors. unchanged
        -> [(Name, Maybe Type, Maybe Expr)]  -- vals. to be closed
        -> [(Name, Type)]  -- GCL constants. binders
        -> [(Name, Type)]  -- GCL constants. binders
        -> [Expr]   -- global properties, to be closed
        -> [Stmt]   -- program, to be closed
        -> [Pragma]  -- pragmas. unchanged.
        -> Program
program types constrs vals consts vars props prog pragma =
   Program types constrs
       (map (third (fmap (close boundvars'))) vals)
       consts vars
       (map (close boundvars') props)
       (map (close boundvars') prog)
       pragma
  where
    boundvars = map (nameToText . fst) (consts ++ vars)
    boundvars' = zip (reverse boundvars) [0..]

----------------------------

-- | Utiilties

extractBinders :: Pattern -> [Text]
extractBinders (PattBinder x _) = [x]
extractBinders (PattWildcard _) = []
extractBinders (PattConstructor _ _ xs) = xs >>= extractBinders

renameBinders :: [Text] -> Pattern -> Pattern
renameBinders ys p = fst (rnBinders ys p)
  where
    rnBinders [] p = (p, [])
    rnBinders (y:ys) (PattBinder _ l)=
       (PattBinder y l, ys)
    rnBinders ys (PattWildcard l) = (PattWildcard l, ys)
    rnBinders ys (PattConstructor c l ps) =
       (PattConstructor c l ps', ys')
      where (ps', ys') = rnLBinders ys ps

    rnLBinders ys [] = ([],ys)
    rnLBinders ys (p:ps) =
        let (p', ys') = rnBinders ys p
            (ps', ys'') = rnLBinders ys' ps
        in (p':ps', ys'')

   -- fresh zs x: generates a variable name that looks like x
   --             but does not appear in zs

fresh :: [Text] -> Text -> Text
fresh zs x = head (dropWhile (`elem` zs) xs)
   where nats = map (pack . show) ([0..] :: [Int])
         xs = x : map (x `append`) nats

   -- freshIds zs xs: calls fresh zs x for each x in xs

freshIds :: [Text] -> [Text] -> [Text]
freshIds zs = map (fresh zs)
