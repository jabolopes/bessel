module Core.IO where

import Prelude hiding (read)

import Data.Functor ((<$>))
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO hiding (hGetContents, hPutStr)
import System.IO.Strict
import System.IO.Unsafe (unsafePerformIO)

import Core
import Data.SrcFile
import Data.Type
import Monad.InterpreterM


handleOp :: Handle -> Int -> IO Expr
handleOp h 0 = boxString <$> hGetContents h


open :: Expr -> Expr
{-# NOINLINE open #-}
open expr =
    TypeExpr "File" $ FnExpr $ \(IntExpr i) -> return $ unsafePerformIO $ do
                                                 h <- openFile (unboxString expr) ReadMode
                                                 val <- handleOp h i
                                                 hClose h
                                                 return val


read :: Expr -> InterpreterM Expr
read (TypeExpr "File" (FnExpr fn)) = fn $ IntExpr 0


srcfile :: SrcFile
srcfile = SrcFile "Core.IO" ["Core"] Nothing $ Right $
          Map.fromList [("open", (ArrowT DynT DynT, m open)),
                        ("read", (ArrowT DynT DynT, FnExpr read))]