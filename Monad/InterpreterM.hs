module Monad.InterpreterM where

import Control.Monad.State
import Data.Dynamic (Typeable, Dynamic)
import qualified Data.Dynamic as Dynamic (fromDynamic, toDyn)
import qualified Data.List as List (intercalate)

import Data.Env (Env)
import qualified Data.Env as Env

data Val
    = BoolVal Bool
    | CharVal Char
    | DynVal Dynamic
    | FnVal (Val -> Val)
    | IntVal Int
    | IOVal (IO Val)
    | RealVal Double
    | SeqVal [Val]
    | TypeVal Val

instance Show Val where
    show (BoolVal False) = "false"
    show (BoolVal True) = "true"
    show (CharVal c) = show c
    show (DynVal val) = "{dyn " ++ show val ++ "}"
    show FnVal {} = "fn"
    show (IntVal i) = show i
    show IOVal {} = "io"
    show (RealVal d) = show d
    show (SeqVal vals)
        | not (null vals) && all isCharVal vals = show $ map (\(CharVal c) -> c) vals
        | otherwise = "[" ++ List.intercalate "," (map show vals) ++ "]"
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

dynVal :: Typeable a => a -> Val
dynVal = DynVal . Dynamic.toDyn

unDynVal :: Typeable a => Val -> Maybe a
unDynVal (DynVal x) = Dynamic.fromDynamic x

boxString :: String -> Val
boxString = SeqVal . map CharVal

isStringVal :: Val -> Bool
isStringVal (SeqVal cs) = all isCharVal cs
isStringVal _ = False

unboxString :: Val -> String
unboxString val@(SeqVal cs) | isStringVal val = map (\(CharVal c) -> c) cs
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
