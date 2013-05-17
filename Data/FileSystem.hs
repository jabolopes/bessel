module Data.FileSystem where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Data.SrcFile (SrcFile)
import qualified Data.SrcFile as SrcFile


data FileSystem
    = FileSystem { files :: Map Int SrcFile
                 , fileIds :: Map String Int }


empty :: FileSystem
empty =
    FileSystem { files = Map.empty
               , fileIds = Map.empty }


initial :: [SrcFile] -> FileSystem
initial = initial' empty
  where initial' fs [] = fs
        initial' fs (srcfile:srcfiles) =
            initial' (add fs srcfile) srcfiles


add :: FileSystem -> SrcFile -> FileSystem
add fs srcfile =
    let
       fid = case Map.lookup (SrcFile.name srcfile) (fileIds fs) of
               Nothing -> Map.size (fileIds fs)
               Just id -> id
    in
      fs { files = Map.insert fid srcfile (files fs)
         , fileIds = Map.insert (SrcFile.name srcfile) fid (fileIds fs) }


get :: FileSystem -> String -> SrcFile
get fs name = files fs Map.! (fileIds fs Map.! name)


toAscList :: FileSystem -> [SrcFile]
toAscList fs = map snd $ Map.toAscList $ files fs


lookup :: FileSystem -> String -> Maybe SrcFile
lookup fs name =
    do fid <- name `Map.lookup` fileIds fs
       fid `Map.lookup` files fs


member :: FileSystem -> String -> Bool
member fs name = name `Map.member` fileIds fs