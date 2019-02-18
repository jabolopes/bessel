module Data.Name where

import qualified Data.Char as Char

import Typechecker.Type (Type)
import qualified Utils

data Name
  = Name { nameStr :: String
         , nameType :: Maybe Type
         }

instance Eq Name where
  Name name1 _ == Name name2 _ =
    name1 == name2

instance Ord Name where
  Name name1 _ `compare` Name name2 _ =
    name1 `compare` name2

instance Show Name where
  show (Name str Nothing) = str
  show (Name str (Just typ)) = str ++ " :: " ++ show typ

isValidName :: String -> Bool
isValidName "" = True
isValidName str = all (/= "") $ Utils.splitId str

empty :: Name
empty = Name "" Nothing

name :: Monad m => String -> Maybe Type -> m Name
name str typ
  | isValidName str = return $ Name str typ
  | otherwise = fail $ "Invalid name " ++ show str

-- TODO: make monadic smart constructor.
untyped :: String -> Name
untyped str =
  case name str Nothing of
    Left err -> error err
    Right x -> x

typed :: Monad m => String -> Type -> m Name
typed str = name str . Just

isEmptyName :: Name -> Bool
isEmptyName = null . nameStr

isTypeName :: Name -> Bool
isTypeName n
  | isEmptyName n = False
  | otherwise = isTypeName' . last . Utils.splitId $ nameStr n
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
