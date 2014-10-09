{-# LANGUAGE TypeOperators, TypeFamilies, DataKinds #-}

module Example.Pass.Depth where

import Data.Generics.Fixplate
import Data.OpenRecords
import Example.Ast

depth :: Label "depth"
depth = Label :: Label "depth"

annDepth :: (r0 :\ "depth",
              (r1 :! "depth") ~ Int,
              ("depth" ::= Int :| r0) ~ r1)
             => AnnExpr r0 -> AnnExpr r1
annDepth = attachDepth 0

attachDepth :: ((r1 :! "depth") ~ Int, ("depth" ::= Int :| r0) ~ r1) =>
               Int -> AnnExpr r0 -> AnnExpr ("depth" ::= Int :| r0)
attachDepth h (Fix (Ann r expr)) = fixAnn (depth := h .| r) expr'
  where expr' = fmap (attachDepth (h+1)) expr

exprDepth :: ((r :! "depth") ~ Int) => Mu (Ann f (Rec r)) -> Int
exprDepth x = attr (unFix x) .! depth
