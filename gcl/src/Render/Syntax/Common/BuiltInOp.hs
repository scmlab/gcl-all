{-# LANGUAGE OverloadedStrings #-}

-- | To replace those instances in Render.Syntax.Common
module Render.Syntax.Common.BuiltInOp where

import Render.Class (Render (..))
import Syntax.Common.BuiltInOp
import Syntax.Parser.Lexer (Tok (..))
import Prelude hiding (Ordering (..))

instance Render Op where
  render EQProp = render (show TokEQProp)
  render EQ = render (show TokEQ)
  render NEQ = render (show TokNEQ)
  render LTE = render (show TokLTE)
  render GTE = render (show TokGTE)
  render LT = render (show TokLT)
  render GT = render (show TokGT)
  render Implies = render (show TokImpl)
  render Conj = render (show TokConj)
  render Disj = render (show TokDisj)
  render Neg = render (show TokNeg)
  render NegNum = render (show TokSub)
  render Add = render (show TokAdd)
  render Sub = render (show TokSub)
  render Mul = render (show TokMul)
  render Div = render (show TokDiv)
  render Mod = render (show TokMod)
  render Max = render (show TokMax)
  render Min = render (show TokMin)
  render Exp = render (show TokExp)
  render Hash = render (show TokHash)
  render Pr = render (show TokComma)
  render ArrUpd = "↣"
  render ArrIdx = "@" -- ChAoS(Render): Is this acceptable?
  render PointsTo = render (show TokPointsTo)
  render SConj = render (show TokSConj)
  render SImp = render (show TokLolipop)
