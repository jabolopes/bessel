{-# LANGUAGE TupleSections #-}
module Data.Source where

import Control.Applicative ((<$>), (<*>))

import Data.Name (Name)
import qualified Data.Name as Name

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

  -- | CharS
  -- @
  -- 'a'
  -- @
  | CharS Char

  -- | CondS
  -- @
  -- x@Int y@isInt = val1
  -- x@Int y@isReal = val2
  -- @
  | CondS [([Source], Source)]

  -- | FnDefS
  -- @
  -- def x = ...
  --
  -- def [x, y] = ...
  -- @
  | FnDefS Source Source

  -- | IdS
  -- @
  -- x
  -- @
  | IdS Name

  -- | IntS
  -- @
  -- 10
  -- @
  | IntS Int

  -- | LetS
  -- @
  -- let
  --   x = ...
  --   y = ...
  -- in
  --   ...
  -- @
  | LetS [Source] Source

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
  -- x@
  -- @
  -- PatS "x" Nothing
  --
  -- @
  --  @
  -- @
  -- PatS "" Nothing
  --
  -- The following does not make sense:
  --   PatS "" (Just ...)
  | PatS String (Maybe Source)

  -- | RealS
  -- @
  -- 10.0
  -- @
  | RealS Double

  -- | SeqS
  -- @
  -- [1, 2, 3]
  -- @
  | SeqS [Source]

  -- | StringS
  -- @
  -- "hello world"
  -- @
  | StringS String

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

  -- | WhereS
  -- @
  -- x where x = 10
  -- @
  | WhereS Source [Source]

appS :: Name -> Source -> Source
appS name = AppS (IdS name)

foldAppS :: Source -> [Source] -> Source
foldAppS = foldr AppS

listToApp :: [Source] -> Source
listToApp = foldl1 AppS

idS :: String -> Source
idS = IdS . Name.untyped

moduleDeps :: Source -> [Name]
moduleDeps (ModuleS _ uses _) = map fst uses
moduleDeps _ = error "Source.moduleDeps: expecting a module"

allPat :: Source
allPat = PatS "" Nothing

isPatAll :: Source -> Bool
isPatAll (PatS "" Nothing) = True
isPatAll _ = False

bindPat :: String -> Source -> Source
bindPat "" src = src
bindPat name (PatS _ (Just src)) = bindPat name src
bindPat name (PatS _ guard) = PatS name guard
bindPat name src = PatS name (Just src)

isTypePat :: Source -> Bool
isTypePat (AppS fn _) = isTypePat fn
isTypePat (PatS name _) = Name.isTypeName $ Name.untyped name
isTypePat (IdS name) = Name.isTypeName name
isTypePat _ = False

appToList :: Source -> [Source]
appToList (AppS fn arg) = appToList fn ++ [arg]
appToList x = [x]

toSource :: Source -> Maybe Source
toSource (AndS src1 src2) = AndS <$> toSource src1 <*> toSource src2
toSource (AppS src1 src2) = AppS <$> toSource src1 <*> toSource src2
toSource (BinOpS op src1 src2) = BinOpS op <$> toSource src1 <*> toSource src2
toSource src@CharS {} = Just src
toSource (CondS ms) = CondS <$> mapM toSource' ms
  where toSource' (args, body) = (args,) <$> toSource body
toSource (FnDefS pat body) = FnDefS pat <$> toSource body
toSource src@IdS {} = Just src
toSource src@IntS {} = Just src
toSource (LetS defns body) = LetS <$> mapM toSource defns <*> toSource body
toSource (ModuleS name uses decls) = ModuleS name uses <$> mapM toSource decls
toSource (OrS src1 src2) = OrS <$> toSource src1 <*> toSource src2
toSource (PatS "" val) = val
toSource (PatS binder Nothing) = return $ idS binder
toSource PatS {} = Nothing
toSource src@RealS {} = Just src
toSource (SeqS srcs) = SeqS <$> mapM toSource srcs
toSource src@StringS {} = Just src
toSource src@TypeDeclS {} = Just src
toSource (WhereS src srcs) = WhereS <$> toSource src <*> mapM toSource srcs
