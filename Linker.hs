module Linker where

import Control.Monad.State
import Data.Functor ((<$>))
import Data.Map (Map)
import qualified Data.Map as Map

import Data.SrcFile
import qualified Data.SrcFile as SrcFile
import Data.Stx
import Data.Symbol
import Monad.InterpreterM


linkSrcFiles :: [SrcFile] -> [SrcFile]
-- linkSrcFiles srcfiles =
--     fst $ runState (mapM linkSrcFileM srcfiles) emptyLinkerState

linkSrcFiles srcfile = srcfile