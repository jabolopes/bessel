module Core.Environment where

import qualified Data.Map as Map (fromList)
import qualified System.Environment as Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)

import Core hiding (srcfile)
import Data.Type
import Data.SrcFile
import Monad.InterpreterM


getArgs :: a -> Expr
{-# NOINLINE getArgs #-}
getArgs _ =
    unsafePerformIO $ do
      args <- Environment.getArgs
      return $ SeqExpr $ map boxString $ tail args


-- edit: fixed undefined
srcfile :: SrcFile
srcfile =
    SrcFile { name = "Core.Environment"
            , deps = ["Core"]
            , frame = Nothing
            , ts = undefined
            , env = undefined
            , srcNs = Right ([], Map.fromList [("getArgs", (ArrowT DynT (SeqT (SeqT CharT)), m getArgs))])
            , renNs = Nothing }


link :: a -> (SrcFile, a)
link ids = (srcfile, ids)