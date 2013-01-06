{-# LANGUAGE DoRec #-}
module Interpreter where

import Control.Monad.State
import Data.Functor ((<$>))
import Data.List (intercalate)

import Data.Env (Env)
import qualified Data.Env as Env
import Data.Stx


type ExprEnv = Env Expr


data Expr
    = BoolExpr Bool
    | IntExpr Int
    | DoubleExpr Double
    | CharExpr Char
    | SeqExpr [Expr]
    | FnExpr (Expr -> InterpreterM Expr)
    | TypeExpr String Expr


instance Show Expr where
    show (BoolExpr False) = "false"
    show (BoolExpr True) = "true"
    show (CharExpr c) = show c
    show (IntExpr i) = show i
    show (DoubleExpr d) = show d

    show (SeqExpr exprs) | not (null exprs) && all isCharExpr exprs =
        show $ map (\(CharExpr c) -> c) exprs

    show (SeqExpr exprs) = "<" ++ intercalate "," (map show exprs) ++ ">"
    show (FnExpr _) = "fn"
    show (TypeExpr name expr) = name ++ " " ++ show expr


isFalseExpr :: Expr -> Bool
isFalseExpr (BoolExpr False) = True
isFalseExpr _ = False


isCharExpr :: Expr -> Bool
isCharExpr (CharExpr _) = True
isCharExpr _ = False


isNotFalseExpr :: Expr -> Bool
isNotFalseExpr = not . isFalseExpr


false :: Expr
false = BoolExpr False


true :: Expr
true = BoolExpr True


stringExpr :: String -> Expr
stringExpr = SeqExpr . map CharExpr


stringLiteral :: Expr -> String
stringLiteral (SeqExpr cs) = map (\(CharExpr c) -> c) cs


all2 :: (a -> b -> Expr) -> [a] -> [b] -> Expr
all2 _ [] _ = false
all2 _ _ [] = false
all2 fn (x1:xs1) (x2:xs2)
    | isNotFalseExpr (fn x1 x2) = all2 fn xs1 xs2
    | otherwise = false


exprEq :: Expr -> Expr -> Expr
exprEq (BoolExpr b1) (BoolExpr b2) = BoolExpr $ b1 == b2
exprEq expr@(IntExpr i1) (IntExpr i2) | i1 == i2 = expr
exprEq expr@(DoubleExpr d1) (DoubleExpr d2) | d1 == d2 = expr
exprEq expr@(CharExpr c1) (CharExpr c2) | c1 == c2 = expr
exprEq expr@(SeqExpr exprs1) (SeqExpr exprs2) | isNotFalseExpr (all2 exprEq exprs1 exprs2) = expr
exprEq expr@(TypeExpr name1 expr1) (TypeExpr name2 expr2) | name1 == name2 = expr1 `exprEq` expr2
exprEq _ _ = false


exprLt :: Expr -> Expr -> Expr
exprLt (BoolExpr b1) (BoolExpr b2) = BoolExpr $ b1 < b2
exprLt expr@(IntExpr i1) (IntExpr i2) = if i1 < i2 then expr else false
exprLt expr@(DoubleExpr d1) (DoubleExpr d2) = if d1 < d2 then expr else false
exprLt expr@(CharExpr c1) (CharExpr c2) = BoolExpr $ c1 < c2
exprLt expr@(SeqExpr exprs1) (SeqExpr exprs2)
      | length exprs1 < length exprs2 = expr
      | length exprs1 == length exprs2 && isNotFalseExpr (all2 exprLt exprs1 exprs2) = expr
      | otherwise = false
exprLt (FnExpr _) (FnExpr _) = false
exprLt (TypeExpr _ _) (TypeExpr _ _) = false
exprLt (BoolExpr _) _ = true
exprLt (IntExpr _) (BoolExpr _) = false
exprLt expr@(IntExpr i) (DoubleExpr d) = if (fromIntegral i) < d then expr else false
exprLt expr@(IntExpr _) _ = expr
exprLt (DoubleExpr _) (BoolExpr _) = false
exprLt expr@(DoubleExpr d) (IntExpr i) = BoolExpr $ d < (fromIntegral i)
exprLt expr@(DoubleExpr _) _ = expr
exprLt (CharExpr _) (BoolExpr _) = false
exprLt (CharExpr _) (IntExpr _) = false
exprLt (CharExpr _) (DoubleExpr _) = false
exprLt expr@(CharExpr _) _ = expr
exprLt (SeqExpr _) (BoolExpr _) = false
exprLt (SeqExpr _) (IntExpr _) = false
exprLt (SeqExpr _) (DoubleExpr _) = false
exprLt (SeqExpr _) (CharExpr _) = false
exprLt expr@(SeqExpr _) _ = expr
exprLt expr@(FnExpr _) (TypeExpr _ _) = expr
exprLt (FnExpr _) _ = false
exprLt (TypeExpr _ _) _ = false


type InterpreterM a = State ExprEnv a


withEnvM :: InterpreterM a -> InterpreterM a
withEnvM m =
    do env <- get
       put $ Env.push env
       val <- m
       put env
       return val


withLexicalEnvM :: Env Expr -> InterpreterM a -> InterpreterM a
withLexicalEnvM env m =
    do env' <- get
       put $ Env.push env
       val <- m
       put env'
       return val


addBindM :: String -> Expr -> InterpreterM ()
addBindM name expr =
    do env <- get
       put $ Env.addBind env name expr


findBindM :: String -> InterpreterM (Maybe Expr)
findBindM name =
    do env <- get
       return $ Env.findBind env name


evalM :: Stx String -> InterpreterM Expr
evalM (CharStx c) = return $ CharExpr c
evalM (IntStx i) = return $ IntExpr i
evalM (DoubleStx d) = return $ DoubleExpr d
evalM (SeqStx stxs) = SeqExpr <$> mapM evalM stxs

evalM (IdStx str) =
    do msym <- findBindM str
       case msym of
         Just expr -> return expr
         Nothing -> error $ "Interpreter.evalM(IdStx): unbound symbols must be caught by the renamer" ++
                            "\n\n\t str = " ++ str ++
                            "\n\n"

evalM (AppStx stx1 stx2) =
    do expr1 <- evalM stx1
       expr2 <- evalM stx2
       case expr1 of
         FnExpr fn -> fn expr2
         _ -> error $ "Interpreter.evalM(AppStx): application of non-functions must be detected by the renamer" ++
                      "\n\n\t stx1 = " ++ show stx1 ++
                      "\n\n\t -> expr1 = " ++ show expr1 ++
                      "\n\n\t stx2 = " ++ show stx2 ++
                      "\n\n\t -> expr2 = " ++ show expr2 ++
                      "\n\n"

evalM (DefnStx Def str body) =
    do rec addBindM str expr
           expr <- evalM body
       return expr

evalM (DefnStx NrDef str body) =
    do expr <- evalM body
       addBindM str expr
       return expr

evalM (LambdaStx str body) =
    FnExpr . closure <$> get
    where closure env expr =
              withLexicalEnvM env $ do
                addBindM str expr
                withEnvM $ evalM body

evalM (ModuleStx prefix (name, stxs)) =
    error $ "Interpreter.evalM(ModuleStx): modules must be flattened by the renamer" ++
            "\n\n\t prefix = " ++ show prefix ++
            "\n\n\t name   = " ++ show name   ++
            "\n\n\t stxs   = " ++ show stxs   ++
            "\n\n"

evalM (TypeStx name stxs) =
    do mapM_ evalM stxs
       return $ SeqExpr $ map CharExpr name

evalM (TypeMkStx name arg) =
    TypeExpr name <$> evalM (IdStx arg)

evalM (TypeUnStx name arg) =
    do TypeExpr _ expr <- evalM $ IdStx arg
       return expr

evalM (TypeIsStx name arg) =
    do expr <- evalM $ IdStx arg
       case expr of
         TypeExpr name' _ | name == name' -> return true
         _ -> return false

evalM (WhereStx stx ("", stxs)) =
    withEnvM $ do
      mapM_ evalM stxs
      withEnvM $ evalM stx

evalM (WhereStx stx (name, stxs)) =
    error $ "Interpreter.evalM(WhereStx): modules must be flattened by the renamer" ++
            "\n\n\t stx  = " ++ show stx  ++
            "\n\n\t name = " ++ show name ++
            "\n\n\t stxs = " ++ show stxs ++
            "\n\n"


interpret :: ExprEnv -> [Stx String] -> (Expr, ExprEnv)
interpret env stxs =
    let
        m = do mapM_ evalM $ init stxs
               evalM $ last stxs
    in
      runState m env

-- interpret env stxs =
--     let
--         m = do mapM_ evalM $ init stxs
--                evalM $ last stxs
--     in
--       case runStateT m env of
--         Left _ -> error "HEHE"
--         Right x -> x