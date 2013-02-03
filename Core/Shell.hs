module Core.Shell where

import Data.List (intercalate)
import qualified Data.Map as Map (fromList)
import System.IO hiding (hGetContents)
import System.IO.Strict (hGetContents)
import System.IO.Unsafe
import System.Process hiding (shell)

import Core hiding (srcfile)
import Data.SrcFile
import Data.Type
import Monad.InterpreterM


shell :: Expr -> Expr
{-# NOINLINE shell #-}
shell expr =
    FnExpr $ \args -> return $
      FnExpr $ \_ -> return $ unsafePerformIO $ do
                       let line = unboxString expr ++ " " ++ unboxString args
                       (hIn, hOut, hErr, p) <- runInteractiveCommand line
                       str <- hGetContents hOut
                       waitForProcess p
                       return $ boxString str


-- edit: fixed undefined
srcfile :: SrcFile
srcfile = SrcFile "Core.Shell" ["Core"] Nothing undefined undefined $ Right $
          ([], Map.fromList [("shell", (ArrowT (SeqT CharT) (SeqT CharT), m shell))])


link :: a -> (SrcFile, a)
link ids = (srcfile, ids)