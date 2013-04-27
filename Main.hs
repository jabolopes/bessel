module Main where

import Data.Functor ((<$>))
import Data.Map (Map)
import qualified Data.Map as Map (fromList)
import System.Environment

import Config
-- import qualified Core.Environment (link)
-- import qualified Core.IO (link)
-- import qualified Core.Shell (link)
-- import qualified Core.Happstack (link)
import Data.Exception
import Data.SrcFile
import qualified Data.SrcFile as SrcFile (name)
import Repl hiding (fs)


links :: [[Int] -> (SrcFile, [Int])]
links = []
    -- [Core.Environment.link,
    --  Core.IO.link,
    --  Core.Shell.link,
    --  Core.Happstack.link]


linkCorefiles :: [Int] -> [[Int] -> (SrcFile, [Int])] -> [SrcFile]
linkCorefiles _ [] = []
linkCorefiles ids (fn:fns) =
    let (srcfile, ids') = fn ids in
    srcfile:linkCorefiles ids' fns


corefiles :: [SrcFile]
corefiles = corefile:linkCorefiles (map (* (-1)) [1..]) links


fs :: Map String SrcFile
fs = Map.fromList $ map (\x -> (SrcFile.name x, x)) corefiles


mainException :: FlException -> IO (Maybe a)
mainException e =
    do flException e
       return Nothing


main :: IO ()
main =
    do -- args <- getArgs
       mstate <- ((Just <$> importFile fs preludeName)
                  `catchFlException` mainException)
       case mstate of
         Nothing -> return ()
         Just state -> repl state