module Data.FileSystem where

import Prelude hiding (lookup, mod)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Data.Definition (Definition)
import Data.Module (Module)
import qualified Data.Module as Module
import Utils

data FileSystem
    = FileSystem { files :: Map Int Module
                 , fileIds :: Map String Int }

empty :: FileSystem
empty =
    FileSystem { files = Map.empty
               , fileIds = Map.empty }

initial :: [Module] -> FileSystem
initial = foldl add empty

add :: FileSystem -> Module -> FileSystem
add fs mod =
  let
    fid = fromMaybe
            (Map.size (fileIds fs))
            (Map.lookup (Module.modName mod) (fileIds fs))
  in
    fs { files = Map.insert fid mod (files fs)
       , fileIds = Map.insert (Module.modName mod) fid (fileIds fs) }

get :: FileSystem -> String -> Module
get fs name =
    case Map.lookup name (fileIds fs) of
      Nothing -> error $ "FileSystem.get: mod " ++ show name ++ " has no id in the filesystem"
      Just modId -> fromMaybe
                      (error $ "Data.FileSystem.get: mod " ++ show name ++ " is not in the filesystem")
                      (Map.lookup modId (files fs))

toAscList :: FileSystem -> [Module]
toAscList fs = map snd $ Map.toAscList $ files fs

lookup :: FileSystem -> String -> Maybe Module
lookup fs name =
    do fid <- name `Map.lookup` fileIds fs
       fid `Map.lookup` files fs

member :: FileSystem -> String -> Bool
member fs name = name `Map.member` fileIds fs

lookupDefinition :: FileSystem -> String -> Maybe Definition
lookupDefinition fs name =
  do let modName = init (splitId name)
     mod <- lookup fs (flattenId modName)
     name `Map.lookup` Module.modDefs mod
