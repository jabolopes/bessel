module Monad.InterpreterM where

import Control.Monad.State
import Data.Dynamic
import Data.List (intercalate)
import Data.Maybe

import Data.Env (Env)
import qualified Data.Env as Env


type ExprEnv = Env Expr


data Expr
    = BoolExpr Bool
    | IntExpr Int
    | DoubleExpr Double
    | CharExpr Char
    | SeqExpr [Expr]
    | FnExpr (Expr -> InterpreterM Expr)
    | TypeExpr String Int Expr
    | DynExpr Dynamic


instance Show Expr where
    show (BoolExpr False) = "false"
    show (BoolExpr True) = "true"
    show (CharExpr c) = show c
    show (IntExpr i) = show i
    show (DoubleExpr d) = show d
    show (SeqExpr exprs)
        | not (null exprs) && all isCharExpr exprs = show $ map (\(CharExpr c) -> c) exprs
        | otherwise = "[" ++ intercalate "," (map show exprs) ++ "]"
    show (FnExpr _) = "fn"
    show (TypeExpr name _ expr) = name ++ " " ++ show expr
    show (DynExpr _) = "#"


isFalseExpr :: Expr -> Bool
isFalseExpr (BoolExpr False) = True
isFalseExpr _ = False


isCharExpr :: Expr -> Bool
isCharExpr (CharExpr _) = True
isCharExpr _ = False


isNotFalseExpr :: Expr -> Bool
isNotFalseExpr = not . isFalseExpr


toDynExpr :: Typeable a => a -> Expr
toDynExpr val = DynExpr $ toDyn val


fromDynExpr :: Typeable a => Expr -> a
fromDynExpr (DynExpr dyn) = fromJust $ fromDynamic dyn


toTypeDynExpr :: Typeable a => String -> Int -> a -> Expr
toTypeDynExpr name tid = TypeExpr name tid . toDynExpr


fromTypeDynExpr :: Typeable a => Int -> Expr -> a
fromTypeDynExpr tid1 (TypeExpr _ tid2 expr)
    | tid1 == tid2 = fromDynExpr expr


false :: Expr
false = BoolExpr False


true :: Expr
true = BoolExpr True


boxString :: String -> Expr
boxString = SeqExpr . map CharExpr


unboxString :: Expr -> String
unboxString (SeqExpr cs) | all isCharExpr cs = map (\(CharExpr c) -> c) cs
unboxString expr =
    error $ "Monad.InterpreterM.unboxString: expecting character sequence" ++
            "\n\n\t expr = " ++ show expr ++ "\n\n"


type InterpreterM a = State ExprEnv a


withEnvM :: InterpreterM a -> InterpreterM a
withEnvM m =
    do env <- get
       withLexicalEnvM env m


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