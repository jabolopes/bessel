module Linker where

import Data.SrcFile (SrcFile)


linkSrcFiles :: [SrcFile] -> [SrcFile]
linkSrcFiles srcfile = srcfile