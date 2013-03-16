module Config (doTypecheck, isPrelude, preludeName, corefile) where

import qualified Core (srcfile)
import Data.SrcFile


doTypecheck :: Bool
doTypecheck = False


preludeNames :: [String]
preludeNames = ["Prelude", "PreludeTypechecker"]


isPrelude :: String -> Bool
isPrelude = (`elem` preludeNames)


preludeName :: String
preludeName | doTypecheck = head $ tail preludeNames
            | otherwise = head preludeNames


corefile :: SrcFile
corefile = Core.srcfile