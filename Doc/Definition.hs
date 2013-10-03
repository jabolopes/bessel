module Doc.Definition where

import Data.List (intercalate)
import Data.Maybe (isNothing)
import Text.PrettyPrint ((<+>), ($+$), equals, empty, isEmpty, text, vcat)

import Data.Definition (Definition(..))
import qualified Data.Definition as Definition
import Doc.Doc (Doc)
import qualified Doc.Doc as Doc
import qualified Doc.Expr as Doc (DocType(..), docExpr)

definitionOk :: String -> String -> [String] -> Doc -> Doc -> Doc -> Doc -> Doc
definitionOk name val freeNames src exp ren err
  | null freeNames =
    hdDoc $+$ srcDoc $+$ expDoc $+$ renDoc $+$ errDoc
  | otherwise =
    hdDoc $+$ freeDoc $+$ srcDoc $+$ expDoc $+$ renDoc $+$ errDoc
  where hdDoc = text name <+> equals <+> text val

        freeDoc =
          Doc.nest $ text "-- free" $+$ text (intercalate ", " freeNames)

        sectionDoc desc expr
          | isEmpty expr = empty
          | otherwise = Doc.nest $ text desc $+$ expr $+$ text ""

        srcDoc = sectionDoc "-- source" src
        expDoc = sectionDoc "-- expanded" exp
        renDoc = sectionDoc "-- renamed" ren
        errDoc = sectionDoc "-- errors" err 

docEither :: Show b => Either a b -> String
docEither (Right x) = show x
docEither _ = "?"

docFree :: Bool -> Definition -> [String]
docFree showFree def
  | showFree = Definition.freeNames def
  | otherwise = []

docSrc :: Bool -> Definition -> Doc
docSrc showSrc Definition { srcExpr = Just expr }
  | showSrc = Doc.docExpr Doc.SrcDocT expr
docSrc _ _ = empty

docExp :: Bool -> Definition -> Doc
docExp showExp Definition { expExpr = Just expr }
  | showExp = Doc.docExpr Doc.ExpDocT expr
docExp _ _ = empty

docRen :: Bool -> Definition -> Doc
docRen showRen Definition { renExpr = Right expr }
  | showRen = Doc.docExpr Doc.RenDocT expr
docRen _ _ = empty

docErrorSrc :: Maybe t -> Doc
docErrorSrc Nothing = text "definition has no source expression"
docErrorSrc _ = empty

docErrorExp :: Maybe t -> Doc
docErrorExp Nothing = text "definition has no expanded expression"
docErrorExp _ = empty

docMaybe :: Either String t -> Doc
docMaybe (Left "") = empty
docMaybe (Left err) = text err
docMaybe _ = empty

docError :: Definition -> Doc
docError def =
  vcat [docErrorSrc (Definition.srcExpr def),
        docErrorExp (Definition.expExpr def),
        docMaybe (Definition.renExpr def),
        docMaybe (Definition.val def)]

docDefn :: Bool -> Bool -> Bool -> Bool -> Definition -> Doc
docDefn showFree showSrc showExp showRen def =
  definitionOk (Definition.name def)
               (docEither (Definition.val def))
               (docFree showFree def)
               (docSrc showSrc def)
               (docExp showExp def)
               (docRen showRen def)
               (docError def)
