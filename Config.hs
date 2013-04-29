module Config (doTypecheck, isPrelude, preludeName, corefile) where

import qualified Core (srcfile)
import Data.SrcFile


doTypecheck :: Bool
doTypecheck = True
-- doTypecheck = False


preludeNames :: [String]
preludeNames = ["Prelude"]


isPrelude :: String -> Bool
isPrelude = (`elem` preludeNames)


preludeName :: String
preludeName = head preludeNames


corefile :: SrcFile
corefile = Core.srcfile