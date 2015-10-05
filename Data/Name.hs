module Data.Name where

import qualified Data.Char as Char

import qualified Utils
import Typechecker.Type (Type)

data Name
  = Name { nameStr:: String
         , nameType :: Maybe Type
         }

instance Eq Name where
  Name name1 _ == Name name2 _ =
    name1 == name2

instance Ord Name where
  Name name1 _ `compare` Name name2 _ =
    name1 `compare` name2

instance Show Name where
  show (Name name Nothing) = name
  show (Name name (Just typ)) = name ++ " :: " ++ show typ

isValidName :: String -> Bool
isValidName "" = True
isValidName str = all (/= "") $ Utils.splitId str

empty :: Name
empty = Name "" Nothing

-- TODO: make monadic smart constructor.
untyped :: String -> Name
untyped str
  | isValidName str = Name str Nothing
  | otherwise = error $ "Invalid name " ++ show str

-- TODO: make monadic smart constructor.
typed :: String -> Type -> Name
typed str typ
  | isValidName str = Name str $ Just typ
  | otherwise = error $ "Invalid name " ++ show str

isEmptyName :: Name -> Bool
isEmptyName = null . nameStr

isTypeName :: Name -> Bool
isTypeName name
  | isEmptyName name = False
  | otherwise = isTypeName' . last . Utils.splitId $ nameStr name
  where
    isTypeName' (x:_) = Char.isUpper x
    isTypeName' _ = False

hasType :: Name -> Bool
hasType Name { nameType = Just _ } = True
hasType _ = False

-- | Joins a module name with a definition name. Definition type (if there is
-- one) is preserved, but module type is dropped.
joinNames :: Name -> Name -> Name
joinNames name1 name2
  | isEmptyName name1 = name2
joinNames Name { nameStr = name1 } Name { nameStr = name2, nameType = typ } =
  Name (name1 ++ "." ++ name2) typ

moduleName :: Name -> Name
moduleName = untyped . Utils.flattenId . init . Utils.splitId . nameStr

definitionName :: Name -> Name
definitionName = untyped . last . Utils.splitId . nameStr
