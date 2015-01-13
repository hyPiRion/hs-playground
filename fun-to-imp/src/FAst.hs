{-# OPTIONS_GHC -Wall -Werror #-}

module FAst (
  FExpr(..),
  FStmt(..),
  frender
  ) where

-- The functional AST

import Text.PrettyPrint

type Ident = String

data FExpr = FInt Int
           | FIdent Ident
           | FCall Ident [FExpr]
           | FFilter FExpr Ident FExpr

data FStmt = FAssign Ident FExpr
           | FIf FExpr [FStmt]

class Pretty e where
  pp :: e -> Doc

bracing :: Pretty a => Doc -> [a] -> Doc
doc `bracing` xs = doc <+> lbrace $+$ (nest 2 $ vcat $ map pp xs) $+$ rbrace

instance Pretty FStmt where
  pp (FAssign ident expr) = text ident <+> char '=' <+> pp expr
  pp (FIf expr stmts) = (text "if" <+> pp expr) `bracing` stmts

instance Pretty FExpr where
  pp (FInt n) = int n
  pp (FIdent ident) = text ident
  pp (FCall ident exprs) = text ident <> lparen <>
                             (hsep $ punctuate comma $ map pp exprs) <> rparen
  pp (FFilter lst ident filterFn) = lparen <> text ident <+> text "<*" <+> pp lst <+>
                                    char '|' $+$ nest 2 (pp filterFn) $+$ rparen

frender :: [FStmt] -> Doc
frender stmts = vcat (map pp stmts)
