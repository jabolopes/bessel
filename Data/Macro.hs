module Data.Macro where

import Control.Applicative ((<$>), (<*>))

import Data.QualName (QualName)
import qualified Data.QualName as QualName (mkQualName)
import Data.TypeName (TypeName)
import qualified Data.TypeName as TypeName (fromTypeName)
import qualified Utils

data PatGuard
  = AllPG
  | PredicatePG Macro
  | ListPG Pat Pat
  | TuplePG [Pat]
  | TypePG TypeName [Pat]

data Pat = Pat { patBinder :: String, patGuard :: PatGuard }

bindPat :: String -> Pat -> Pat
bindPat name pat = pat { patBinder = name }

allPat :: Pat
allPat =
  Pat { patBinder = ""
      , patGuard = AllPG }

predicatePat :: Macro -> Pat
predicatePat macro =
  Pat { patBinder = ""
      , patGuard = PredicatePG macro }

listPat :: Pat -> Pat -> Pat
listPat hdPat tlPat =
  Pat { patBinder = ""
      , patGuard = ListPG hdPat tlPat }

tuplePat :: [Pat] -> Pat
tuplePat pats =
  Pat { patBinder = ""
      , patGuard = TuplePG pats }

typePat :: TypeName -> [Pat] -> Pat
typePat typeName pats =
  Pat { patBinder = ""
      , patGuard  = TypePG typeName pats }

toMacro :: Pat -> Maybe Macro
toMacro pat
  | null (patBinder pat) = toMacro' (patGuard pat)
  | otherwise = Nothing
  where toMacro' AllPG = Nothing
        toMacro' (PredicatePG macro) = Just macro
        toMacro' ListPG {} = Nothing
        toMacro' (TuplePG pats) = SeqM <$> mapM toMacro pats
        toMacro' (TypePG typeName pats) =
          do let fn = idM . TypeName.fromTypeName $ typeName
             args <- mapM toMacro pats
             Just $ foldl1 AppM (fn:args)

data Constructor = Constructor TypeName Pat

data Macro
  -- | AndM
  -- @
  -- x && y
  -- @
  = AndM Macro Macro

  -- | AppM
  -- @
  -- f x
  -- @
  | AppM Macro Macro

  -- | BinOpM
  -- @
  -- x + y
  -- x - y
  -- @
  | BinOpM String Macro Macro

  -- | CharM
  -- @
  -- 'a'
  -- @
  | CharM Char

  -- | CondM
  -- @
  -- x@Int y@isInt = val1
  -- x@Int y@isReal = val2
  -- @
  | CondM [([Pat], Macro)]

  -- | FnDeclM
  -- @
  -- def x = ...
  -- @
  | FnDeclM String Macro

  -- | IdM
  -- @
  -- x
  -- @
  | IdM QualName

  -- | IntM
  -- @
  -- 10
  -- @
  | IntM Int

  -- | ModuleM
  -- @
  -- me Module
  --
  -- use X
  -- use Y as Z
  --
  -- ...
  -- @
  | ModuleM String [(String, String)] [Macro]

  -- | OrM
  -- @
  -- x || y
  -- @
  | OrM Macro Macro

  -- | RealM
  -- @
  -- 10.0
  -- @
  | RealM Double

  -- | SeqM
  -- @
  -- [1, 2, 3]
  -- @
  | SeqM [Macro]

  -- | StringM
  -- @
  -- "hello world"
  -- @
  | StringM String

  -- | Constructor
  -- @
  -- type Type
  --   | Cons isInt
  -- @
  --
  -- @
  -- type Type
  --   | Cons [isInt, isReal]
  -- @
  | TypeDeclM TypeName [Constructor]

  -- | WhereM
  -- @
  -- x where x = 10
  -- @
  | WhereM Macro [Macro]

appM :: String -> Macro -> Macro
appM name = AppM (idM name)

idM :: String -> Macro
idM = IdM . QualName.mkQualName . Utils.splitId

moduleDeps :: Macro -> [String]
moduleDeps (ModuleM _ uses _) = map fst uses
