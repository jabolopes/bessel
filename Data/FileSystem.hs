module Data.FileSystem
       ( FileSystem
       , empty
       , initial
       , add
       , toAscList
       , lookup
       , member
       , lookupDefinition
       ) where

import Prelude hiding (lookup, mod)

import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Name (Name)
import qualified Data.Name as Name

import Data.Definition (Definition)
import Data.Module (Module)
import qualified Data.Module as Module
import Data.Result (Result)
import qualified Data.PrettyString as PrettyString

data FileSystem
  = FileSystem { fsModules :: Map Int Module
               , fsModuleIds :: Map Name Int
               }

empty :: FileSystem
empty =
  FileSystem { fsModules = Map.empty
             , fsModuleIds = Map.empty
             }

initial :: [Module] -> FileSystem
initial = foldl add empty

add :: FileSystem -> Module -> FileSystem
add fs mod =
  let
    fid = fromMaybe
            (Map.size (fsModuleIds fs))
            (Map.lookup (Module.modName mod) (fsModuleIds fs))
  in
    fs { fsModules = Map.insert fid mod (fsModules fs)
       , fsModuleIds = Map.insert (Module.modName mod) fid (fsModuleIds fs)
       }

toAscList :: FileSystem -> [Module]
toAscList fs = map snd $ Map.toAscList $ fsModules fs

lookup :: FileSystem -> Name -> Maybe Module
lookup fs name =
  do fid <- name `Map.lookup` fsModuleIds fs
     fid `Map.lookup` fsModules fs

member :: FileSystem -> Name -> Bool
member fs name = name `Map.member` fsModuleIds fs

lookupDefinition :: FileSystem -> Name -> Result Definition
lookupDefinition fs name =
  let
    modName = Name.moduleName name
    defName = Name.definitionName name
  in
   case lookup fs modName of
     Nothing -> throwError . PrettyString.text $ "module " ++ show modName ++ " not found"
     Just mod ->
       case Name.nameStr name `Map.lookup` Module.modDefs mod of
         Nothing -> throwError . PrettyString.text $ "definition " ++ show defName ++ " not found in module " ++ show modName
         Just def -> return def
