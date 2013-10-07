module Pretty.Definition where

import Data.List (intercalate)

import Data.Definition (Definition(..))
import qualified Data.Definition as Definition
import Data.PrettyString (PrettyString, (<>), (<+>), ($+$))
import qualified Data.PrettyString as PrettyString
import qualified Pretty.Expr as Pretty (DocType(..), docExpr)

definitionOk :: String -> String -> [String] -> PrettyString -> PrettyString -> PrettyString -> PrettyString -> PrettyString
definitionOk name val freeNames src exp ren err =
  hdDoc $+$ PrettyString.vcat [freeDoc, srcDoc, expDoc, renDoc, errDoc]
  where hdDoc = PrettyString.text name <+> PrettyString.equals <+> PrettyString.text val

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

docEither :: Show b => Either a b -> String
docEither (Right x) = show x
docEither _ = "?"

docFree :: Bool -> Definition -> [String]
docFree showFree def
  | showFree = Definition.defFreeNames def
  | otherwise = []

docSrc :: Bool -> Definition -> PrettyString
docSrc showSrc Definition { defSrc = Just expr }
  | showSrc = Pretty.docExpr Pretty.SrcDocT expr
docSrc _ _ = PrettyString.empty

docExp :: Bool -> Definition -> PrettyString
docExp showExp Definition { defExp = Just expr }
  | showExp = Pretty.docExpr Pretty.ExpDocT expr
docExp _ _ = PrettyString.empty

docRen :: Bool -> Definition -> PrettyString
docRen showRen Definition { defRen = Right expr }
  | showRen = Pretty.docExpr Pretty.RenDocT expr
docRen _ _ = PrettyString.empty

docErrorSrc :: Maybe t -> PrettyString
docErrorSrc Nothing = PrettyString.text "definition has no source expression"
docErrorSrc _ = PrettyString.empty

docErrorExp :: Maybe t -> PrettyString
docErrorExp Nothing = PrettyString.text "definition has no expanded expression"
docErrorExp _ = PrettyString.empty

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
               (docEither (Definition.defVal def))
               (docFree showFree def)
               (docSrc showSrc def)
               (docExp showExp def)
               (docRen showRen def)
               (docError def)
