module Core.Math where

import qualified Prelude 

import Data.List (intercalate)
import qualified Data.Map as Map (fromList)
import System.IO hiding (hGetContents)
import System.IO.Strict (hGetContents)
import System.IO.Unsafe
import System.Process hiding (math)

import Core hiding (fnDesc, srcfile)
import Data.SrcFile
import Data.Type
import Monad.InterpreterM


cos :: Val -> Val
cos (DoubleVal d) = DoubleVal (Prelude.cos d)


sin :: Val -> Val
sin (DoubleVal d) = DoubleVal (Prelude.sin d)


fnDesc :: FnDesc
fnDesc = [("cos", ArrowT DoubleT DoubleT, m cos)
         ,("sin", ArrowT DoubleT DoubleT, m sin)]


srcfile :: SrcFile
srcfile = mkCoreSrcFile "Core.Math" ["Core"] [] fnDesc


link :: a -> (SrcFile, a)
link ids = (srcfile, ids)