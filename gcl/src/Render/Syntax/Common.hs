module Render.Syntax.Common where

import Render.Class
import Syntax.Common
import Syntax.Parser.Lexer
import Prelude hiding (Ordering (..))

instance Render Name where
  render (Name n l) = tempHandleMaybeRange l (render n)

instance Render ChainOp where
  render (EQProp l) = tempHandleMaybeRange l $ render (show TokEQProp)
  render (EQPropU l) = tempHandleMaybeRange l $ render (show TokEQPropU)
  render (EQ l) = tempHandleMaybeRange l $ render (show TokEQ)
  render (NEQ l) = tempHandleMaybeRange l $ render (show TokNEQ)
  render (NEQU l) = tempHandleMaybeRange l $ render (show TokNEQU)
  render (LTE l) = tempHandleMaybeRange l $ render (show TokLTE)
  render (LTEU l) = tempHandleMaybeRange l $ render (show TokLTEU)
  render (GTE l) = tempHandleMaybeRange l $ render (show TokGTE)
  render (GTEU l) = tempHandleMaybeRange l $ render (show TokGTEU)
  render (LT l) = tempHandleMaybeRange l $ render (show TokLT)
  render (GT l) = tempHandleMaybeRange l $ render (show TokGT)

instance Render ArithOp where
  render (Implies l) = tempHandleMaybeRange l $ render (show TokImpl)
  render (ImpliesU l) = tempHandleMaybeRange l $ render (show TokImplU)
  render (Conj l) = tempHandleMaybeRange l $ render (show TokConj)
  render (ConjU l) = tempHandleMaybeRange l $ render (show TokConjU)
  render (Disj l) = tempHandleMaybeRange l $ render (show TokDisj)
  render (DisjU l) = tempHandleMaybeRange l $ render (show TokDisjU)
  render (Neg l) = tempHandleMaybeRange l $ render (show TokNeg)
  render (NegU l) = tempHandleMaybeRange l $ render (show TokNegU)
  render (NegNum l) = tempHandleMaybeRange l $ render (show TokSub)
  render (Add l) = tempHandleMaybeRange l $ render (show TokAdd)
  render (Sub l) = tempHandleMaybeRange l $ render (show TokSub)
  render (Mul l) = tempHandleMaybeRange l $ render (show TokMul)
  render (Div l) = tempHandleMaybeRange l $ render (show TokDiv)
  render (Mod l) = tempHandleMaybeRange l $ render (show TokMod)
  render (Max l) = tempHandleMaybeRange l $ render (show TokMax)
  render (Min l) = tempHandleMaybeRange l $ render (show TokMin)
  render (Exp l) = tempHandleMaybeRange l $ render (show TokExp)
  render (Hash l) = tempHandleMaybeRange l $ render (show TokHash)
  render (PointsTo l) = tempHandleMaybeRange l $ render (show TokPointsTo)
  render (SConj l) = tempHandleMaybeRange l $ render (show TokSConj)
  render (SImp l) = tempHandleMaybeRange l $ render (show TokLolipop)

instance Render TypeOp where
  render (Arrow l) = tempHandleMaybeRange l $ render (show TokArrowU)

instance Render Op where
  render (ChainOp op) = render op
  render (ArithOp op) = render op
  render (TypeOp op) = render op
