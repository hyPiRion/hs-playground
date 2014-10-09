{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Example.Ast where

import Data.Generics.Fixplate
import Data.OpenRecords

data Expr e = LBool Bool
            | LInt Int
            | If e e e
            | Cmp e e
            | Add e e
            | Not e
              deriving (Show, Read, Functor, Foldable, Traversable)

type RawExpr = Attr Expr (Rec Empty)

fixAnn :: a -> f (Mu (Ann f a)) -> Attr f a
fixAnn a x = Fix $ Ann a x

fixEmpty :: f (Mu (Ann f (Rec Empty))) -> Attr f (Rec Empty)
fixEmpty = fixAnn empty

iLBool :: Bool -> RawExpr
iLBool b = fixEmpty $ LBool b

iLInt :: Int -> RawExpr
iLInt i = fixEmpty $ LInt i

iIf :: RawExpr -> RawExpr -> RawExpr -> RawExpr
iIf e1 e2 e3 = fixEmpty $ If e1 e2 e3

iCmp :: RawExpr -> RawExpr -> RawExpr
iCmp e1 e2 = fixEmpty $ Cmp e1 e2

iAdd :: RawExpr -> RawExpr -> RawExpr
iAdd e1 e2 = fixEmpty $ Add e1 e2

iNot :: RawExpr -> RawExpr
iNot e = fixEmpty $ Not e
