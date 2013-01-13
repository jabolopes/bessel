module Core.IO where

import Prelude hiding (read)

import Data.Dynamic
import Data.Functor ((<$>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import System.IO hiding (hGetContents, hPutStr)
import System.IO.Strict
import System.IO.Unsafe (unsafePerformIO)

import Core
import Data.SrcFile
import Data.Type
import Monad.InterpreterM


handleOp :: Handle -> Int -> IO Expr
handleOp h 0 = boxString <$> hGetContents h


fileTypename = "Core.IO.File"


open :: Expr -> InterpreterM Expr
{-# NOINLINE open #-}
open expr =
    return $ unsafePerformIO $ do
      h <- openFile (unboxString expr) ReadMode
      return $ TypeExpr fileTypename $ DynExpr $ toDyn h


read :: Expr -> Expr
{-# NOINLINE read #-}
read (TypeExpr tname (DynExpr d)) | tname == fileTypename =
    unsafePerformIO $ boxString <$> hGetContents (fromJust (fromDynamic d))


close :: Expr -> Expr
{-# NOINLINE close #-}
close (TypeExpr tname (DynExpr d)) | tname == fileTypename =
    unsafePerformIO $ do
      hClose $ fromJust $ fromDynamic d
      return $ SeqExpr []


srcfile :: SrcFile
srcfile = SrcFile "Core.IO" ["Core"] Nothing $ Right $
          Map.fromList [("open", (ArrowT DynT DynT, FnExpr open)),
                        ("read", (ArrowT DynT DynT, m read)),
                        ("close", (ArrowT DynT DynT, m close))]