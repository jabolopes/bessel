{-# LANGUAGE TupleSections #-}
module Data.Source where

import Prelude hiding ((<>))

import Control.Applicative ((<$>), (<*>))

import Data.Literal (Literal(..))
import Data.Name (Name)
import qualified Data.Name as Name
import Typechecker.Type (Type)

data Source
  -- | AndS
  -- @
  -- x && y
  -- @
  = AndS Source Source

  -- | AppS
  -- @
  -- f x
  -- @
  | AppS Source Source

  -- | BinOpS
  -- @
  -- x + y
  -- x - y
  -- @
  | BinOpS String Source Source

  -- | CondS
  -- @
  -- x@Int y@isInt = val1
  -- x@Int y@isReal = val2
  -- @
  | CondS [([Source], Source)]

  -- | FnDefS pat type body whereClause
  -- @pat@ must be a pattern, which includes 'IdS', 'PatS', or any kind of
  -- 'Source' that makes sense for a pattern.
  -- @type@ is an optional type annotation for the body.
  -- @body@ must be a 'Source' that evaluates to a value.
  -- @defns@ must contains 'FnDefS' and corresponds to a where clause.
  --
  -- Examples:
  -- @
  -- let x = ...
  --
  -- let [x, y] = ...
  --
  -- let x = ...
  -- where
  --   let y = ...
  -- @
  | FnDefS Source (Maybe Type) Source [Source]

  -- | IdS
  -- @
  -- x
  -- @
  | IdS Name

  -- | LetS
  -- @
  -- let
  --   x = ...
  --   y = ...
  -- in
  --   ...
  -- @
  | LetS [Source] Source

  -- | LiteralS
  -- @
  -- 'a'
  -- 1
  -- 2.0
  -- "three"
  -- @
  | LiteralS Literal

  -- | ModuleS
  -- @
  -- me Module
  --
  -- use X
  -- use Y as Z
  --
  -- ...
  -- @
  | ModuleS Name [(Name, Name)] [Source]

  -- | OrM
  -- @
  -- x || y
  -- @
  | OrS Source Source

  -- | PatS
  -- @
  -- x@isInt
  -- @
  -- PatS "x" (Just (IdS "isInt"))
  --
  -- @
  -- x
  -- @
  -- PatS "x" Nothing
  --
  -- @
  --  @
  -- @
  -- PatS "" Nothing
  --
  -- The following does not make sense because it simplifies to 'Source'.
  --   PatS "" (Just ...)
  | PatS Name (Maybe Source)

  -- | SeqS
  -- @
  -- [1, 2, 3]
  -- @
  | SeqS [Source]

  -- | TupleS
  -- @
  -- (1, 2.0, "ola")
  -- @
  | TupleS [Source]

  -- | TypeDeclS
  -- @
  -- type Type
  --   | Cons isInt
  -- @
  --
  -- @
  -- type Type
  --   | Cons [isInt, isReal]
  -- @
  | TypeDeclS Name [(Name, Source)]

appS :: Name -> Source -> Source
appS name = AppS (IdS name)

listToApp :: [Source] -> Source
listToApp = foldl1 AppS

appToList :: Source -> [Source]
appToList (AppS fn arg) = appToList fn ++ [arg]
appToList x = [x]

idS :: String -> Source
idS = IdS . Name.untyped

moduleDeps :: Source -> [Name]
moduleDeps (ModuleS _ uses _) = map fst uses
moduleDeps _ = error "Source.moduleDeps: expecting a module"

allPat :: Source
allPat = PatS Name.empty Nothing

isPatAll :: Source -> Bool
isPatAll (PatS binder Nothing) = Name.isEmptyName binder
isPatAll _ = False

bindPat :: Name -> Source -> Source
bindPat binder src
  | Name.isEmptyName binder = src
bindPat binder (PatS _ (Just src)) = bindPat binder src
bindPat binder (PatS _ guard) = PatS binder guard
bindPat binder src = PatS binder (Just src)

intS :: Int -> Source
intS = LiteralS . IntL

stringS :: String -> Source
stringS = LiteralS . StringL

isTypePat :: Source -> Bool
isTypePat (AppS fn _) = isTypePat fn
isTypePat (IdS name) = Name.isTypeName name
isTypePat _ = False

toSource :: Source -> Maybe Source
toSource (AndS src1 src2) = AndS <$> toSource src1 <*> toSource src2
toSource (AppS src1 src2) = AppS <$> toSource src1 <*> toSource src2
toSource (BinOpS op src1 src2) = BinOpS op <$> toSource src1 <*> toSource src2
toSource (CondS ms) = CondS <$> mapM toSource' ms
  where toSource' (args, body) = (args,) <$> toSource body
toSource (FnDefS pat typ body defns) =
  FnDefS pat typ <$> toSource body <*> mapM toSource defns
toSource src@IdS {} = Just src
toSource (LetS defns body) = LetS <$> mapM toSource defns <*> toSource body
toSource src@LiteralS {} = Just src
toSource (ModuleS name uses decls) = ModuleS name uses <$> mapM toSource decls
toSource (OrS src1 src2) = OrS <$> toSource src1 <*> toSource src2
toSource (PatS binder val)
  | Name.isEmptyName binder = val
toSource (PatS binder Nothing) = return $ IdS binder
toSource PatS {} = Nothing
toSource (SeqS srcs) = SeqS <$> mapM toSource srcs
toSource (TupleS srcs) = TupleS <$> mapM toSource srcs
toSource src@TypeDeclS {} = Just src
