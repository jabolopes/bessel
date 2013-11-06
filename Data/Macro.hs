module Data.Macro where

import Data.QualName (QualName)
import qualified Data.QualName as QualName (mkQualName)
import qualified Utils

data PatGuard
  = AllPG
  | PredicatePG Macro
  | ListPG Pat Pat
  | TuplePG [Pat]

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
