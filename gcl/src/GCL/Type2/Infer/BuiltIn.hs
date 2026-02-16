module GCL.Type2.Infer.BuiltIn where

import GCL.Type2.Types
import qualified Syntax.Abstract.Types as A
import Syntax.Common.Types (ArithOp (..), ChainOp (..))
import Prelude hiding (EQ, GT, LT)

getArithOpType :: ArithOp -> TIMonad A.Type
getArithOpType Implies {} = return (typeBool `typeToType` typeBool `typeToType` typeBool)
getArithOpType ImpliesU {} = return (typeBool `typeToType` typeBool `typeToType` typeBool)
getArithOpType Conj {} = return (typeBool `typeToType` typeBool `typeToType` typeBool)
getArithOpType ConjU {} = return (typeBool `typeToType` typeBool `typeToType` typeBool)
getArithOpType Disj {} = return (typeBool `typeToType` typeBool `typeToType` typeBool)
getArithOpType DisjU {} = return (typeBool `typeToType` typeBool `typeToType` typeBool)
getArithOpType Neg {} = return (typeBool `typeToType` typeBool)
getArithOpType NegU {} = return (typeBool `typeToType` typeBool)
getArithOpType NegNum {} = return (typeInt `typeToType` typeInt)
getArithOpType Add {} = return (typeInt `typeToType` typeInt `typeToType` typeInt)
getArithOpType Sub {} = return (typeInt `typeToType` typeInt `typeToType` typeInt)
getArithOpType Mul {} = return (typeInt `typeToType` typeInt `typeToType` typeInt)
getArithOpType Div {} = return (typeInt `typeToType` typeInt `typeToType` typeInt)
getArithOpType Mod {} = return (typeInt `typeToType` typeInt `typeToType` typeInt)
getArithOpType Max {} = return (typeInt `typeToType` typeInt `typeToType` typeInt)
getArithOpType Min {} = return (typeInt `typeToType` typeInt `typeToType` typeInt)
getArithOpType Exp {} = return (typeInt `typeToType` typeInt `typeToType` typeInt)
getArithOpType Hash {} = return (typeBool `typeToType` typeInt)
getArithOpType PointsTo {} = return (typeInt `typeToType` typeInt `typeToType` typeInt)
getArithOpType SConj {} = return (typeBool `typeToType` typeBool `typeToType` typeBool)
getArithOpType SImp {} = return (typeBool `typeToType` typeBool `typeToType` typeBool)

getChainOpType :: ChainOp -> TIMonad A.Type
getChainOpType EQProp {} = return (typeBool `typeToType` typeBool `typeToType` typeBool)
getChainOpType EQPropU {} = return (typeBool `typeToType` typeBool `typeToType` typeBool)
getChainOpType EQ {} = do
  ftv <- freshTVar
  return (ftv `typeToType` ftv `typeToType` typeBool)
getChainOpType NEQ {} = do
  ftv <- freshTVar
  return (ftv `typeToType` ftv `typeToType` typeBool)
getChainOpType NEQU {} = do
  ftv <- freshTVar
  return (ftv `typeToType` ftv `typeToType` typeBool)
getChainOpType LT {} = return (typeInt `typeToType` typeInt `typeToType` typeBool)
getChainOpType LTE {} = return (typeInt `typeToType` typeInt `typeToType` typeBool)
getChainOpType LTEU {} = return (typeInt `typeToType` typeInt `typeToType` typeBool)
getChainOpType GT {} = return (typeInt `typeToType` typeInt `typeToType` typeBool)
getChainOpType GTE {} = return (typeInt `typeToType` typeInt `typeToType` typeBool)
getChainOpType GTEU {} = return (typeInt `typeToType` typeInt `typeToType` typeBool)
