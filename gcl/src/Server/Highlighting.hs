{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Server.Highlighting
  ( Highlighting,
    collectHighlighting,
  )
where

import Control.Monad.RWS
import Data.Foldable (toList)
import GCL.Range
  ( MaybeRanged (maybeRangeOf),
    posCol,
    posLine,
    rangeSpan,
    rangeStart,
  )
import qualified Hack
import qualified Language.LSP.Protocol.Types as J
import Server.IntervalMap
  ( Collect (..),
    M,
    runM,
  )
import qualified Server.IntervalMap as IntervalMap
import Syntax.Common
import Syntax.Concrete

type Highlighting = J.SemanticTokenAbsolute

collectHighlighting :: Program -> [Highlighting]
collectHighlighting program =
  toList $ runM mempty (collect program :: M () Highlighting ())

--------------------------------------------------------------------------------
-- helper function for converting some syntax node to Highlighting
addHighlighting ::
  (MaybeRanged a) =>
  J.SemanticTokenTypes ->
  [J.SemanticTokenModifiers] ->
  a ->
  M () Highlighting ()
addHighlighting types modifiers node = case maybeRangeOf node of
  Nothing -> return ()
  Just range ->
    tell $
      IntervalMap.singleton range $
        J.SemanticTokenAbsolute
          (Hack.intToUInt (posLine (rangeStart range) - 1))
          (Hack.intToUInt (posCol (rangeStart range) - 1))
          (Hack.intToUInt (rangeSpan range))
          types
          modifiers

--------------------------------------------------------------------------------
-- newtypes for giving common datatype like `Name` different interpretations

newtype AsConstructor = AsConstructor Name

instance Collect () Highlighting AsConstructor where
  collect (AsConstructor a) = addHighlighting J.SemanticTokenTypes_EnumMember [] a

newtype AsVariable = AsVariable Name

instance Collect () Highlighting AsVariable where
  collect (AsVariable a) = addHighlighting J.SemanticTokenTypes_Variable [] a

newtype AsName = AsName Name

instance Collect () Highlighting AsName where
  collect (AsName a) = addHighlighting J.SemanticTokenTypes_Function [] a

--------------------------------------------------------------------------------
-- Program

instance Collect () Highlighting Program where
  collect (Program as bs) = do
    collect as
    collect bs

--------------------------------------------------------------------------------
-- Definition

instance Collect () Highlighting DefinitionBlock where
  collect (DefinitionBlock _tokA as _tokB) = collect as

instance Collect () Highlighting Definition where
  collect (TypeDefn tokData name binders _tokDef bs) = do
    addHighlighting J.SemanticTokenTypes_Keyword [] tokData
    addHighlighting J.SemanticTokenTypes_Type [] name
    addHighlighting J.SemanticTokenTypes_Parameter [] binders
    collect bs
  collect (FuncDefnSig a b) = do
    collect a
    collect b
  collect (FuncDefn a bs _tok c) = do
    addHighlighting J.SemanticTokenTypes_Function [J.SemanticTokenModifiers_Declaration] a
    collect (fmap AsVariable bs)
    collect c

--------------------------------------------------------------------------------
-- Declaration

instance Collect () Highlighting Declaration where
  collect (ConstDecl tok a) = do
    addHighlighting J.SemanticTokenTypes_Keyword [] tok
    collect a
  collect (VarDecl tok a) = do
    addHighlighting J.SemanticTokenTypes_Keyword [] tok
    collect a

instance Collect () Highlighting TypeDefnCtor where
  collect (TypeDefnCtor name _) = do
    collect (AsName name)

instance Collect () Highlighting DeclBase where
  collect (DeclBase as _ b) = do
    collect (fmap AsVariable as)
    collect b

instance Collect () Highlighting DeclProp where
  collect (DeclProp _tokA a _tokB) = collect a

instance Collect () Highlighting DeclType where
  collect (DeclType a b) = do
    collect a
    collect b

--------------------------------------------------------------------------------
-- SemanticTokenModifiers_t

instance Collect () Highlighting Stmt where
  collect :: Stmt -> M () Highlighting ()
  collect = \case
    Skip x -> addHighlighting J.SemanticTokenTypes_Keyword [] x
    Abort x -> addHighlighting J.SemanticTokenTypes_Keyword [] x
    Assign as tok bs -> do
      collect (fmap AsVariable as)
      addHighlighting J.SemanticTokenTypes_Keyword [] tok
      collect bs
    AAssign a _ b _ tok c -> do
      collect (AsVariable a)
      addHighlighting J.SemanticTokenTypes_Keyword [] tok
      collect b
      collect c
    Assert _ a _ -> collect a
    LoopInvariant _ a _ tok _ b _ -> do
      collect a
      addHighlighting J.SemanticTokenTypes_Keyword [] tok
      collect b
    Do tokA as tokB -> do
      addHighlighting J.SemanticTokenTypes_Keyword [] tokA
      collect as
      addHighlighting J.SemanticTokenTypes_Keyword [] tokB
    If tokA as tokB -> do
      addHighlighting J.SemanticTokenTypes_Keyword [] tokA
      collect as
      addHighlighting J.SemanticTokenTypes_Keyword [] tokB
    SpecQM _ -> return ()
    Spec tokA _ tokB -> do
      addHighlighting J.SemanticTokenTypes_Keyword [] tokA
      addHighlighting J.SemanticTokenTypes_Keyword [] tokB
    Proof _ _ _ range -> do
      addHighlighting J.SemanticTokenTypes_Keyword [] range
    -- addHighlighting J.SemanticTokenTypes_Keyword [] tokA
    -- addHighlighting J.SemanticTokenTypes_Keyword [] tokB
    Alloc a tok tokNew _ bs _ -> do
      collect (AsVariable a)
      addHighlighting J.SemanticTokenTypes_Keyword [] tok
      addHighlighting J.SemanticTokenTypes_Keyword [J.SemanticTokenModifiers_Modification] tokNew
      collect bs
    HLookup a tok tokStar b -> do
      collect (AsVariable a)
      addHighlighting J.SemanticTokenTypes_Keyword [] tok
      addHighlighting J.SemanticTokenTypes_Keyword [J.SemanticTokenModifiers_Modification] tokStar
      collect b
    HMutate tokStar a tok b -> do
      addHighlighting J.SemanticTokenTypes_Keyword [J.SemanticTokenModifiers_Modification] tokStar
      collect a
      addHighlighting J.SemanticTokenTypes_Keyword [] tok
      collect b
    Dispose tok a -> do
      addHighlighting J.SemanticTokenTypes_Keyword [] tok
      collect a
    -- TODO:
    Block {} -> return ()

instance Collect () Highlighting GdCmd where
  collect (GdCmd a tok bs) = do
    collect a
    addHighlighting J.SemanticTokenTypes_Macro [] tok
    collect bs

--------------------------------------------------------------------------------

instance Collect () Highlighting Expr where
  collect = \case
    Paren _ a _ -> collect a
    Lit a -> collect a
    Var a -> collect (AsVariable a)
    Const a -> collect (AsVariable a)
    Op a -> addHighlighting J.SemanticTokenTypes_Operator [] a
    Chain ch -> case ch of
      Pure expr -> collect expr
      More ch' op expr -> do
        collect (Chain ch')
        addHighlighting J.SemanticTokenTypes_Operator [] op
        collect expr
    Arr a _ b _ -> do
      collect a
      collect b
    App a@App {} b -> do
      collect a
      collect b
    App (Const a) b -> do
      collect (AsName a)
      collect b
    App a b -> do
      collect a
      collect b
    Quant tokA op names tokB a tokC b tokD -> do
      addHighlighting J.SemanticTokenTypes_Keyword [] tokA
      addHighlighting J.SemanticTokenTypes_Operator [] op
      collect (map AsVariable names)
      addHighlighting J.SemanticTokenTypes_Keyword [] tokB
      collect a
      addHighlighting J.SemanticTokenTypes_Keyword [] tokC
      collect b
      addHighlighting J.SemanticTokenTypes_Keyword [] tokD
    Case tokA expr tokB cases -> do
      addHighlighting J.SemanticTokenTypes_Keyword [] tokA
      collect expr
      addHighlighting J.SemanticTokenTypes_Keyword [] tokB
      collect cases
    HoleQM _ -> return ()
    Hole tokA _ tokB -> do
      addHighlighting J.SemanticTokenTypes_Keyword [] tokA
      addHighlighting J.SemanticTokenTypes_Keyword [] tokB

instance Collect () Highlighting CaseClause where
  collect (CaseClause _ arrow body) = do
    addHighlighting J.SemanticTokenTypes_Macro [] arrow
    collect body

instance Collect () Highlighting Lit where
  collect x = addHighlighting J.SemanticTokenTypes_Number [] x

--------------------------------------------------------------------------------

instance Collect () Highlighting EndpointOpen where
  collect (IncludingOpening tok a) = do
    addHighlighting J.SemanticTokenTypes_Keyword [] tok
    collect a
  collect (ExcludingOpening tok a) = do
    addHighlighting J.SemanticTokenTypes_Keyword [] tok
    collect a

instance Collect () Highlighting EndpointClose where
  collect (IncludingClosing a tok) = do
    collect a
    addHighlighting J.SemanticTokenTypes_Keyword [] tok
  collect (ExcludingClosing a tok) = do
    collect a
    addHighlighting J.SemanticTokenTypes_Keyword [] tok

instance Collect () Highlighting Interval where
  collect (Interval a tok b) = do
    collect a
    addHighlighting J.SemanticTokenTypes_Keyword [] tok
    collect b

instance Collect () Highlighting TBase where
  collect = addHighlighting J.SemanticTokenTypes_Type []

instance Collect () Highlighting Type where
  collect (TParen _ a _) = collect a
  collect (TBase a) = collect a
  collect (TArray tokArray a tokOf b) = do
    addHighlighting J.SemanticTokenTypes_Keyword [] tokArray
    collect a
    addHighlighting J.SemanticTokenTypes_Keyword [] tokOf
    collect b
  collect (TOp op) = addHighlighting J.SemanticTokenTypes_Operator [] op
  collect (TData name _) = addHighlighting J.SemanticTokenTypes_Type [] name
  collect (TApp a b) = do
    collect a
    collect b
  collect (TMetaVar name _) = addHighlighting J.SemanticTokenTypes_Type [] name
