module Core.IO where

import Prelude hiding (read)

import Data.Dynamic
import Data.Functor ((<$>))
import qualified Data.Map as Map (empty, fromList)
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


link :: [Int] -> (SrcFile, [Int])
link ids = (srcfile, tail ids)
    where fileTypename = "Core.IO.File"
          fileTid = head ids

          open :: Expr -> InterpreterM Expr
          {-# NOINLINE open #-}
          open expr =
              return $ unsafePerformIO $ do
                h <- openFile (unboxString expr) ReadMode
                return $ TypeExpr fileTypename fileTid $ DynExpr $ toDyn h

          read :: Expr -> Expr
          {-# NOINLINE read #-}
          read (TypeExpr _ tid (DynExpr dyn)) | tid == fileTid =
              unsafePerformIO $ boxString <$> hGetContents (fromJust (fromDynamic dyn))

          close :: Expr -> Expr
          {-# NOINLINE close #-}
          close (TypeExpr _ tid (DynExpr dyn)) | tid == fileTid =
              unsafePerformIO $ do
                hClose $ fromJust $ fromDynamic dyn
                return $ SeqExpr []

          -- edit: fixed undefined
          srcfile :: SrcFile
          srcfile = SrcFile "Core.IO" ["Core"] Nothing undefined undefined $ Right
                    (["File"],
                     Map.fromList [("open", (ArrowT DynT DynT, FnExpr open)),
                                   ("read", (ArrowT DynT DynT, m read)),
                                   ("close", (ArrowT DynT DynT, m close))])