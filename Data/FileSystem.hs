module Data.FileSystem where

import Prelude hiding (lookup)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Data.Definition (Definition)
import Data.SrcFile (SrcFile)
import qualified Data.SrcFile as SrcFile
import Utils


data FileSystem
    = FileSystem { files :: Map Int SrcFile
                 , fileIds :: Map String Int }


empty :: FileSystem
empty =
    FileSystem { files = Map.empty
               , fileIds = Map.empty }


initial :: [SrcFile] -> FileSystem
initial = foldl add empty


add :: FileSystem -> SrcFile -> FileSystem
add fs srcfile =
    let
       fid = fromMaybe
               (Map.size (fileIds fs))
               (Map.lookup (SrcFile.name srcfile) (fileIds fs))
    in
      fs { files = Map.insert fid srcfile (files fs)
         , fileIds = Map.insert (SrcFile.name srcfile) fid (fileIds fs) }


get :: FileSystem -> String -> SrcFile
get fs name =
    case Map.lookup name (fileIds fs) of
      Nothing -> error $ "FileSystem.get: srcfile " ++ show name ++ " has no id in the filesystem"
      Just srcfileId -> fromMaybe
                          (error $ "FileSystem.get: srcfile " ++ show name ++ " is not in the filesystem")
                          (Map.lookup srcfileId (files fs))


toAscList :: FileSystem -> [SrcFile]
toAscList fs = map snd $ Map.toAscList $ files fs


lookup :: FileSystem -> String -> Maybe SrcFile
lookup fs name =
    do fid <- name `Map.lookup` fileIds fs
       fid `Map.lookup` files fs


member :: FileSystem -> String -> Bool
member fs name = name `Map.member` fileIds fs


definition :: FileSystem -> String -> Definition
definition fs name =
    let srcfileName:_ = splitId name in
    SrcFile.defs (get fs srcfileName) Map.! name


lookupDefinition :: FileSystem -> String -> Maybe Definition
lookupDefinition fs name =
  do let srcfileName:_:_ = splitId name
     srcfile <- lookup fs srcfileName
     name `Map.lookup` SrcFile.defs srcfile