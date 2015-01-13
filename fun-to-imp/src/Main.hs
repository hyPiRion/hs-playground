{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

import FAst
import IAst

import Control.Lens hiding (Level)
import Control.Monad.Reader
import Control.Monad.State
import Data.Map as M
import Data.Set as S

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)

catted :: [FStmt]
catted = [FAssign "x" (FFilter
                        (FFilter (FIdent "lst")
                         "z" (FCall "eq" [FCall "len" [FIdent "z"],
                                          FCall "len" [FIdent "lst"]]))
                        "w"
                        (FCall "greater" [FCall "len" [FIdent "w"],
                                          FInt 10]))]

nested :: [FStmt]
nested = [FAssign "x" (FFilter (FIdent "y") "x"
                       $ FCall "eq" [FCall "len"
                                     [FFilter (FCall "attr" [FIdent "x"])
                                      "z"
                                      $ FCall "eq" [ FCall "len" [FIdent "x"]
                                                   , FCall "len" [FIdent "z"]]
                                     , (FCall "attr" [FIdent "x"])]])]

type Level = Int
data AstEnv = AstEnv { _requiredVars :: Set String
                     , _blockLevel :: Level }
            deriving (Eq, Show)
makeLenses ''AstEnv

data AstState = AstState { _varTable :: Map String Level }
makeLenses ''AstState

type PassM = ReaderT AstEnv (StateT AstState Identity)
type PassVal a = PassM a
type Pass a b = a -> PassVal b

gensym :: Pass String String
gensym s = do vt <- gets _varTable
              level <- asks _blockLevel
              required <- asks _requiredVars
              let s' = gensym' required vt level s
              modify (varTable %~ M.insert s' level)
              return s'

gensym' :: Set String -> Map String Level -> Int -> String -> String
gensym' required vt level s
  | isOk s = s
  | otherwise = checkIncr 2
  where checkIncr :: Int -> String
        checkIncr n = let sn = s ++ '_' : show n in
                       if isOk sn then sn
                       else checkIncr (n + 1)
        isOk x = M.lookup x vt /= Just level && S.notMember x required

newBlock :: Pass (PassVal a) a
newBlock blockAction = do vt <- gets _varTable
                          res <- local (blockLevel %~ (+ 1)) blockAction
                          modify (varTable .~ vt)
                          return res

-- This is NOT sufficient, you should probably do some alpha-renaming or
-- something other clever tricks to ensure the identifiers doesn't overlap.
require :: [String] -> Pass (PassVal a) a
require names = local (requiredVars %~ insertAll names)
  where insertAll ns s = Prelude.foldl (flip S.insert) s ns

convertExpr :: Pass FExpr ([IStmt], IExpr)
convertExpr (FInt n) = return ([], IInt n)
convertExpr (FIdent ident) = return ([], IIdent ident)
convertExpr (FCall ident args) = do argConv <- mapM convertExpr args
                                    return (concatMap fst argConv,
                                            ICall ident (Prelude.map snd argConv))
convertExpr (FFilter lst ident filterFn)
  = do lstIdent <- gensym (ident ++ "_list")
       (lstStmts, convList) <- convertExpr lst
       inFor <- newBlock $ require [ident] $ genFor lstIdent
       let for = IFor ident convList inFor
       let lstCreate = IAssign lstIdent $ ICall "empty" [convList]
       return (lstStmts ++ [lstCreate, for], IIdent lstIdent)
    where genFor lstIdent = do (filterStmts, filterExpr) <- convertExpr filterFn
                               let ifExpr = [IIf filterExpr
                                             [IAssign lstIdent
                                              $ ICall "insert" [IIdent lstIdent,
                                                                IIdent ident]]]
                               return $ filterStmts ++ ifExpr

convertStmt :: Pass FStmt [IStmt]
convertStmt (FAssign ident expr)
  = do (expStmts, convExpr) <- convertExpr expr
       return $ expStmts ++ [IAssign ident convExpr]
convertStmt (FIf expr stmts)
  = do (expStmts, convExpr) <- convertExpr expr
       inIf <- newBlock $ concatMapM convertStmt stmts
       return $ expStmts ++ [IIf convExpr inIf]

-- here you'd usually add in init state and so forth.
runConversion :: [FStmt] -> [IStmt]
runConversion stmts = fst $ runIdentity
                      $ runStateT (runReaderT convAction initEnv) initState
  where convAction = concatMapM convertStmt stmts
        initState = AstState { _varTable = M.empty }
        initEnv = AstEnv { _requiredVars = S.empty
                         , _blockLevel = 0 }


printConversion :: [FStmt] -> IO ()
printConversion xs
  = do putStrLn $ show $ frender xs
       putStrLn "-->"
       putStrLn $ show $ irender $ runConversion xs

main :: IO ()
main = do printConversion catted
          putStrLn "\n===\n"
          printConversion nested
