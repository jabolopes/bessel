module Config (doTypecheck, preludeName, isPrelude, corefile) where

import qualified Core (srcfile)
import qualified CoreTypechecker (srcfile)
import Data.SrcFile


doTypecheck :: Bool
doTypecheck = True


preludeNames :: [String]
preludeNames = ["Prelude", "PreludeTypechecker"]


isPrelude :: String -> Bool
isPrelude = (`elem` preludeNames)


preludeName :: String
preludeName | doTypecheck = head $ tail preludeNames
            | otherwise = head preludeNames


corefiles :: [SrcFile]
corefiles = [Core.srcfile, CoreTypechecker.srcfile]


corefile :: SrcFile
corefile | doTypecheck = head $ tail corefiles
         | otherwise = head corefiles