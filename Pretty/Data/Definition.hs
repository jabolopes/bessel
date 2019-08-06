module Pretty.Data.Definition (docDefn) where

import Prelude hiding (exp)

import Data.List (intercalate)

import Data.Definition (Definition(..))
import qualified Data.Definition as Definition
import Data.PrettyString (PrettyString, (<+>), ($+$))
import qualified Data.PrettyString as PrettyString
import Data.Name (Name)
import qualified Data.Name as Name
import qualified Pretty.Data.Expr as Pretty (docExpr)
import qualified Pretty.Data.Source as Pretty

definitionOk :: Name -> String -> [String] -> PrettyString -> PrettyString -> PrettyString -> PrettyString -> PrettyString
definitionOk name val freeNames src exp ren err =
  hdDoc $+$ PrettyString.vcat [freeDoc, srcDoc, expDoc, renDoc, errDoc]
  where
    hdDoc = PrettyString.text (show name) <+> PrettyString.equals <+> PrettyString.text val

    freeDoc
      | null freeNames = PrettyString.empty
      | otherwise = PrettyString.nest $ PrettyString.text "-- free" $+$ PrettyString.text (intercalate ", " freeNames)

    sectionDoc desc expr
      | PrettyString.isEmpty expr = PrettyString.empty
      | otherwise = PrettyString.nest $ PrettyString.text desc $+$ expr

    srcDoc = sectionDoc "-- source" src
    expDoc = sectionDoc "-- expanded" exp
    renDoc = sectionDoc "-- renamed" ren
    errDoc = sectionDoc "-- errors" err

-- TODO: This should display the value, not just ref, but we need to
-- be in the IO monad.
docVal :: Either a b -> String
docVal (Right _) = "ref"
docVal _ = "?"

docFree :: Bool -> Definition -> [String]
docFree showFree def
  | showFree = map Name.nameStr $ Definition.defFreeNames def
  | otherwise = []

docSource :: Bool -> Definition -> PrettyString
docSource showSrc Definition { defSrc = Right src }
  | showSrc = Pretty.docSource src
docSource _ _ = PrettyString.empty

docExp :: Bool -> Definition -> PrettyString
docExp showExp Definition { defExp = Right expr }
  | showExp = Pretty.docExpr expr
docExp _ _ = PrettyString.empty

docRen :: Bool -> Definition -> PrettyString
docRen showRen Definition { defRen = Right expr }
  | showRen = Pretty.docExpr expr
docRen _ _ = PrettyString.empty

docErrorSrc :: Either PrettyString a -> PrettyString
docErrorSrc (Right _) = PrettyString.empty
docErrorSrc (Left err)
  | PrettyString.isEmpty err = PrettyString.text "definition has no source expression"
  | otherwise = err

docErrorExp :: Either PrettyString a -> PrettyString
docErrorExp (Right _) = PrettyString.empty
docErrorExp (Left err)
  | PrettyString.isEmpty err = PrettyString.text "definition has no expanded expression"
  | otherwise = err

docErrorRen :: Either PrettyString a -> PrettyString
docErrorRen (Left err) = err
docErrorRen _ = PrettyString.empty

docErrorVal :: Either String a -> PrettyString
docErrorVal (Left "") = PrettyString.empty
docErrorVal (Left err) = PrettyString.text err
docErrorVal _ = PrettyString.empty

docError :: Definition -> PrettyString
docError def =
  PrettyString.vcat [docErrorSrc (Definition.defSrc def),
                     docErrorExp (Definition.defExp def),
                     docErrorRen (Definition.defRen def),
                     docErrorVal (Definition.defVal def)]

docDefn :: Bool -> Bool -> Bool -> Bool -> Definition -> PrettyString
docDefn showFree showSrc showExp showRen def =
  definitionOk (Definition.defName def)
               (docVal (Definition.defVal def))
               (docFree showFree def)
               (docSource showSrc def)
               (docExp showExp def)
               (docRen showRen def)
               (docError def)
