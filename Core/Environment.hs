module Core.Environment where

import qualified Data.Map as Map
import qualified System.Environment as Environment
import System.IO.Unsafe

import Core
import Data.Type
import Data.SrcFile
import Monad.InterpreterM


getArgs :: a -> Expr
{-# NOINLINE getArgs #-}
getArgs _ =
    unsafePerformIO $ do
      args <- Environment.getArgs
      return $ SeqExpr $ map boxString args


srcfile :: SrcFile
srcfile = SrcFile "Core.Environment" ["Core"] Nothing $ Right $
          Map.fromList [("getArgs", (ArrowT DynT (SeqT (SeqT CharT)), m getArgs))]