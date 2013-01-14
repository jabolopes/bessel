module Core.Environment where

import qualified Data.Map as Map (fromList)
import qualified System.Environment as Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)

import Core
import Data.Type
import Data.SrcFile
import Monad.InterpreterM


getArgs :: a -> Expr
{-# NOINLINE getArgs #-}
getArgs _ =
    unsafePerformIO $ do
      args <- Environment.getArgs
      return $ SeqExpr $ map boxString $ tail args


srcfile :: SrcFile
srcfile = SrcFile "Core.Environment" ["Core"] Nothing $ Right $
          Map.fromList [("getArgs", (ArrowT DynT (SeqT (SeqT CharT)), m getArgs))]