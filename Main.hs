module Main where

import Data.Functor ((<$>))
import Data.Map (Map)
import qualified Data.Map as Map (fromList)
import System.Environment

import Config
-- import qualified Core.Environment (link)
-- import qualified Core.IO (link)
import qualified Core.Shell (link)
-- import qualified Core.Happstack (link)
import Data.Exception
import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem (initial)
import Data.SrcFile
import qualified Data.SrcFile as SrcFile (name)
import Repl hiding (initialFs)


links :: [[Int] -> (SrcFile, [Int])]
links = [
    -- Core.Environment.link,
    --  Core.IO.link,
    Core.Shell.link]
    --  Core.Happstack.link


linkCorefiles :: [Int] -> [[Int] -> (SrcFile, [Int])] -> [SrcFile]
linkCorefiles _ [] = []
linkCorefiles ids (fn:fns) =
    let (srcfile, ids') = fn ids in
    srcfile:linkCorefiles ids' fns


corefiles :: [SrcFile]
corefiles = corefile:linkCorefiles (map (* (-1)) [1..]) links


mainException :: UserException -> IO (Maybe a)
mainException e =
    do putUserException e
       return Nothing


main :: IO ()
main =
    do -- args <- getArgs
       mstate <- (Just <$> importFile (FileSystem.initial corefiles) preludeName)
                  `catchUserException` mainException
       case mstate of
         Nothing -> return ()
         Just state -> repl state