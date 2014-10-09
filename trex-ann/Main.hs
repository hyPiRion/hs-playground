{-# LANGUAGE TypeOperators, TypeFamilies, DataKinds #-}

import Data.OpenRecords
import Example.Ast
import Example.Pass.Typecheck

e :: RawExpr
e = iIf (iNot (iCmp (iLInt 10) (iLInt 20)))
    (iAdd (iLInt 3) (iLInt 6))
    (iLInt 0)

eTyped :: AnnExpr ("type'" ::= Type :| Empty)
eTyped = inferType e

main :: IO ()
main = print e >> print eTyped
