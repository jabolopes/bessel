module Core.Shell where

import Data.List (intercalate)
import qualified Data.Map as Map (fromList)
import System.IO hiding (hGetContents)
import System.IO.Strict (hGetContents)
import System.IO.Unsafe
import System.Process hiding (shell)

import Core hiding (fnDesc, srcfile)
import Data.SrcFile
import Data.Type
import Monad.InterpreterM


shell :: Val -> Val
{-# NOINLINE shell #-}
shell expr =
    FnVal $ \args -> return $
      FnVal $ \_ -> return $ unsafePerformIO $ do
                      let line = unboxString expr ++ " " ++ unboxString args
                      (hIn, hOut, hErr, p) <- runInteractiveCommand line
                      str <- hGetContents hOut
                      waitForProcess p
                      return $ boxString str


fnDesc :: FnDesc
fnDesc = [("shell", ForallT "a"
                      (ArrowT (SeqT CharT)
                       (ArrowT (SeqT CharT)
                        (ArrowT (VarT "a") (SeqT CharT)))), m shell)]


srcfile :: SrcFile
srcfile = mkCoreSrcFile "Core.Shell" ["Core"] [] fnDesc


link :: a -> (SrcFile, a)
link ids = (srcfile, ids)