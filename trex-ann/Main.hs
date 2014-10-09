{-# LANGUAGE TypeOperators, TypeFamilies, DataKinds #-}

import Data.OpenRecords
import Example.Ast
import Example.Pass.Typecheck
import Example.Pass.Depth

exampleExpr :: RawExpr
exampleExpr = iIf (iNot (iCmp (iLInt 10) (iLInt 20)))
    (iAdd (iLInt 3) (iLInt 6))
    (iLInt 0)

-- Better leave these type signatures out when possible. They aren't immediately
-- obvious, and depends on ordering when they're as free as they are here.

-- Another option I will check out is avoiding the :\ constraints, if that helps.
nanopass :: ((("depth" ::= Int :| "type'" ::= Type :| r0) :! "depth") ~ Int,
             (("type'" ::= Type :| r0) :! "type'") ~ Type,
             r0 :\ "type'",
             ("type'" ::= Type :| r0) :\ "depth")
             => AnnExpr r0 -> AnnExpr ("depth" ::= Int :| "type'" ::= Type :| r0)
nanopass = annDepth . inferType
nanopass2 = inferType . annDepth

-- Although this is illegal according to our constraints, the type checker
-- cannot confirm this because, well, I don't know. But the last constraint is
-- clearly illegal (The others are okay)
broken :: ((("depth" ::= Int :| "depth" ::= Int :| r0) :! "depth") ~ Int,
           (("depth" ::= Int :| r0) :! "depth") ~ Int,
           r0 :\ "depth",
           ("depth" ::= Int :| r0) :\ "depth")
          => AnnExpr r0 -> AnnExpr ("depth" ::= Int :| "depth" ::= Int :| r0)
broken e = annDepth (annDepth e)

-- This fails at compile time:
-- tryBroken = broken exampleExpr

annotated :: AnnExpr ("depth" ::= Int :| "type'" ::= Type :| Empty)
annotated = nanopass exampleExpr -- this signature matches nanopass2 as well

main :: IO ()
main = do print exampleExpr
          print $ nanopass exampleExpr
          print $ nanopass2 exampleExpr == nanopass exampleExpr
