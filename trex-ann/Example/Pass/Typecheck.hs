{-# LANGUAGE TypeOperators, TypeFamilies, DataKinds #-}

module Example.Pass.Typecheck where

import Data.Generics.Fixplate
import Data.OpenRecords
import Example.Ast

data Type = TInt | TBool
          deriving (Eq)

instance Show Type where
  show TInt = "Int"
  show TBool = "Bool"

type' :: Label "type'"
type' = Label :: Label "type'"
-- quote at end to avoid conflict with the "type" keyword

inferType :: (r0 :\ "type'",
              (r1 :! "type'") ~ Type,
              ("type'" ::= Type :| r0) ~ r1)
             => AnnExpr r0 -> AnnExpr r1
inferType = cata attachType

attachType :: ((r1 :! "type'") ~ Type, ("type'" ::= Type :| r0) ~ r1) =>
              Ann Expr (Rec r0) (AnnExpr r1)
               -> AnnExpr ("type'" ::= Type :| r0)
attachType (Ann r (LBool b)) = fixAnn (type' := TBool .| r) $ LBool b
attachType (Ann r (LInt i)) = fixAnn (type' := TInt .| r) $ LInt i
attachType (Ann r (Add e1 e2))
  | exprType e1 == TInt && sameType e1 e2 = fixAnn (type' := TInt .| r) $ Add e1 e2
  | otherwise = error "addition error: one of the summands was not of type Int"
attachType (Ann r (If e1 e2 e3))
  | exprType e1 /= TBool = error "If expr has non-boolean conditional"
  | not (sameType e2 e3) = error "then and else clause has differing types"
  | otherwise = fixAnn (type' := exprType e2 .| r) $ If e1 e2 e3
attachType (Ann r (Cmp e1 e2))
  | not (sameType e1 e2) = error "attempts to compare differernt types"
  | otherwise = fixAnn (type' := TBool .| r) $ Cmp e1 e2
attachType (Ann r (Not e))
  | exprType e /= TBool = error "Expression for not is not a boolean"
  | otherwise = fixAnn (type' := TBool .| r) $ Not e

sameType :: (r :! "type'") ~ Type => AnnExpr r -> AnnExpr r -> Bool
sameType a b = exprType a == exprType b

exprType ::  ((r :! "type'") ~ Type) => Mu (Ann f (Rec r)) -> Type
exprType x = attr (unFix x) .! type'
