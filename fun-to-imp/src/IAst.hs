{-# OPTIONS_GHC -Wall -Werror #-}

module IAst (
  IExpr(..),
  IStmt(..),
  irender
  ) where

-- The functional AST

import Text.PrettyPrint

type Ident = String

data IExpr = IInt Int
           | IIdent Ident
           | ICall Ident [IExpr]

data IStmt = IAssign Ident IExpr
           | IIf IExpr [IStmt]
           | IFor Ident IExpr [IStmt]

class Pretty e where
  pp :: e -> Doc

bracing :: Pretty a => Doc -> [a] -> Doc
doc `bracing` xs = doc <+> lbrace $+$ (nest 2 $ vcat $ map pp xs) $+$ rbrace

instance Pretty IStmt where
  pp (IAssign ident expr) = text ident <+> char '=' <+> pp expr
  pp (IIf expr stmts) = (text "if" <+> pp expr) `bracing` stmts
  pp (IFor ident e ss) = (text "for" <+> text ident <+> text "in" <+> pp e)
                         `bracing` ss

instance Pretty IExpr where
  pp (IInt n) = int n
  pp (IIdent ident) = text ident
  pp (ICall ident exprs) = text ident <> lparen <>
                             (hsep $ punctuate comma $ map pp exprs) <> rparen

irender :: [IStmt] -> Doc
irender stmts = vcat (map pp stmts)


