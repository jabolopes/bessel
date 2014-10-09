{-# LANGUAGE TupleSections #-}
module Data.Source where

import Control.Applicative ((<$>), (<*>))

import Data.QualName (QualName)
import qualified Data.QualName as QualName (mkQualName)
import qualified Utils

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

  -- | FnDeclS
  -- @
  -- def x = ...
  -- @
  | FnDeclS String Source

  -- | IdS
  -- @
  -- x
  -- @
  | IdS QualName

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
  | ModuleS String [(String, String)] [Source]

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
  | TypeDeclS QualName [(QualName, Source)]

  -- | WhereS
  -- @
  -- x where x = 10
  -- @
  | WhereS Source [Source]

appS :: String -> Source -> Source
appS name = AppS (idS name)

listToApp :: [Source] -> Source
listToApp = foldl1 AppS

idS :: String -> Source
idS = IdS . QualName.mkQualName . Utils.splitId

moduleDeps :: Source -> [String]
moduleDeps (ModuleS _ uses _) = map fst uses

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
toSource (FnDeclS name body) = FnDeclS name <$> toSource body
toSource src@IdS {} = Just src
toSource src@IntS {} = Just src
toSource (LetS defns body) = LetS <$> mapM toSource defns <*> toSource body
toSource (ModuleS name uses decls) = ModuleS name uses <$> mapM toSource decls
toSource (OrS src1 src2) = OrS <$> toSource src1 <*> toSource src2
toSource (PatS "" val) = val
toSource PatS {} = Nothing
toSource src@RealS {} = Just src
toSource (SeqS srcs) = SeqS <$> mapM toSource srcs
toSource src@StringS {} = Just src
toSource src@TypeDeclS {} = Just src
toSource (WhereS src srcs) = WhereS <$> toSource src <*> mapM toSource srcs
