module Monad.InterpreterM where

import Control.Monad.State
import Data.Array (Array)
import qualified Data.Array as Array
import Data.Dynamic (Typeable, Dynamic)
import qualified Data.Dynamic as Dynamic (fromDynamic, toDyn)
import Data.IORef
import qualified Data.List as List (intercalate)

import Data.Env (Env)
import qualified Data.Env as Env

data Val
    = BoolVal Bool
    | CharVal Char
    | DynVal Dynamic
    | FnVal (Val -> InterpreterM Val)
    | IntVal Int
    | IOVal (IO Val)
    | RealVal Double
    | SeqVal [Val]
    | StringVal String
    | TupleVal (Array Int Val)
    | TypeVal Val
    | VariantVal Int Int (Maybe Val)

instance Show Val where
    show (BoolVal False) = "false"
    show (BoolVal True) = "true"
    show (CharVal c) = show c
    show (DynVal val) = "{dyn " ++ show val ++ "}"
    show FnVal {} = "fn"
    show (IntVal i) = show i
    show IOVal {} = "io"
    show (RealVal d) = show d
    show (SeqVal vals) = "[" ++ List.intercalate "," (map show vals) ++ "]"
    show (StringVal s) = show s
    show (TupleVal vals) = "(" ++ List.intercalate ", " (map show $ Array.elems vals) ++ ")"
    show (TypeVal val) = "{" ++ show val ++ "}"
    show (VariantVal _ cons val) = "+" ++ show cons ++ " " ++ show val

isFalseVal :: Val -> Bool
isFalseVal (BoolVal False) = True
isFalseVal _ = False

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
unDynVal val =
  error $ "Monad.InterpreterM.unDynVal: expecting dynamic value" ++
          "\n\n\t val = " ++ show val ++ "\n\n"

primitive :: (Val -> Val) -> Val
primitive fn = FnVal $ return . fn

type InterpreterEnv = Env (IORef Val)
type InterpreterM a = StateT InterpreterEnv IO a

withEmptyEnvM :: InterpreterM a -> InterpreterM a
withEmptyEnvM m =
  do env <- get
     put Env.empty
     val <- m
     put env
     return val

withEnvM :: InterpreterM a -> InterpreterM a
withEnvM m =
  do env <- get
     withLexicalEnvM env m

withLexicalEnvM :: InterpreterEnv -> InterpreterM a -> InterpreterM a
withLexicalEnvM env m =
  do env' <- get
     put $ Env.push env
     val <- m
     put env'
     return val

addBindM :: String -> IORef Val -> InterpreterM ()
addBindM name val =
  do env <- get
     put $ Env.addBind env name val

replaceBindM :: String -> Val -> InterpreterM ()
replaceBindM name val =
  do env <- get
     case Env.findBind env name of
       Nothing -> error $ "Bind " ++ name ++ " does not exist"
       Just ref -> liftIO (writeIORef ref val)

findBindM :: String -> InterpreterM (Maybe (IORef Val))
findBindM name =
  do env <- get
     return $ Env.findBind env name
