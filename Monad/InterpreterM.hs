module Monad.InterpreterM where

import Control.Monad.State
import Data.Dynamic
import Data.Functor ((<$>))
import Data.List (intercalate)
import Data.Maybe

import Data.Env (Env)
import qualified Data.Env as Env

type ValEnv = Env Val


data Val
    = BoolVal Bool
    | IntVal Int
    | DoubleVal Double
    | CharVal Char
    | SeqVal [Val]
    | FnVal (Val -> InterpreterM Val)
    | TypeVal Val
    | DynVal Dynamic


instance Show Val where
    show (BoolVal False) = "false"
    show (BoolVal True) = "true"
    show (CharVal c) = show c
    show (IntVal i) = show i
    show (DoubleVal d) = show d
    show (SeqVal vals)
        | not (null vals) && all isCharVal vals = show $ map (\(CharVal c) -> c) vals
        | otherwise = "[" ++ intercalate "," (map show vals) ++ "]"
    show (FnVal _) = "fn"
    show (TypeVal val) = "{" ++ show val ++ "}"
    show (DynVal _) = "#"


isFalseVal :: Val -> Bool
isFalseVal (BoolVal False) = True
isFalseVal _ = False


isCharVal :: Val -> Bool
isCharVal CharVal {} = True
isCharVal _ = False


isNotFalseVal :: Val -> Bool
isNotFalseVal = not . isFalseVal


toDynVal :: Typeable a => a -> Val
toDynVal val = DynVal $ toDyn val


fromDynVal :: Typeable a => Val -> a
fromDynVal (DynVal dyn) = fromJust $ fromDynamic dyn


false :: Val
false = BoolVal False


true :: Val
true = BoolVal True


boxString :: String -> Val
boxString = SeqVal . map CharVal


unboxString :: Val -> String
unboxString (SeqVal cs) | all isCharVal cs = map (\(CharVal c) -> c) cs
unboxString val =
    error $ "Monad.InterpreterM.unboxString: expecting character sequence" ++
            "\n\n\t val = " ++ show val ++ "\n\n"


type InterpreterM a = State ValEnv a


withEnvM :: InterpreterM a -> InterpreterM a
withEnvM m =
    do env <- get
       withLexicalEnvM env m


withLexicalEnvM :: Env Val -> InterpreterM a -> InterpreterM a
withLexicalEnvM env m =
    do env' <- get
       put $ Env.push env
       val <- m
       put env'
       return val


addBindM :: String -> Val -> InterpreterM ()
addBindM name val =
    do env <- get
       put $ Env.addBind env name val


findBindM :: String -> InterpreterM (Maybe Val)
findBindM name =
    do env <- get
       return $ Env.findBind env name