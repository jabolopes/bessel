module Monad.InterpreterM where

import Control.Monad.State
import qualified Data.List as List (intercalate)

import Data.Env (Env)
import qualified Data.Env as Env

data Val
    = BoolVal Bool
    | IntVal Int
    | RealVal Double
    | CharVal Char
    | SeqVal [Val]
    | FnVal (Val -> InterpreterM Val)
    | TypeVal Val

instance Show Val where
    show (BoolVal False) = "false"
    show (BoolVal True) = "true"
    show (CharVal c) = show c
    show (IntVal i) = show i
    show (RealVal d) = show d
    show (SeqVal vals)
        | not (null vals) && all isCharVal vals = show $ map (\(CharVal c) -> c) vals
        | otherwise = "[" ++ List.intercalate "," (map show vals) ++ "]"
    show (FnVal _) = "fn"
    show (TypeVal val) = "{" ++ show val ++ "}"

isFalseVal :: Val -> Bool
isFalseVal (BoolVal False) = True
isFalseVal _ = False

isCharVal :: Val -> Bool
isCharVal CharVal {} = True
isCharVal _ = False

isNotFalseVal :: Val -> Bool
isNotFalseVal = not . isFalseVal

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

type InterpreterM a = State (Env Val) a

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
