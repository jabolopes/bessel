module Doc.Definition where

import Data.List (intercalate)
import Data.Maybe (isNothing)
import Text.PrettyPrint ((<+>), ($+$), equals, empty, isEmpty, text)

import Data.Definition (Definition(..))
import qualified Data.Definition as Definition
import Doc.Doc (Doc)
import qualified Doc.Doc as Doc
import qualified Doc.Expr as Doc (DocType(..), docExpr)

definitionOk :: String -> String -> [String] -> Doc -> Doc -> Doc -> Doc
definitionOk name val freeNames src exp ren
  | null freeNames =
    hdDoc $+$ srcDoc $+$ expDoc $+$ renDoc
  | otherwise =
    hdDoc $+$ freeDoc $+$ srcDoc $+$ expDoc $+$ renDoc
  where hdDoc = text name <+> equals <+> text val

        freeDoc =
          Doc.nest $ text "-- free" $+$ text (intercalate ", " freeNames)

        exprDoc desc expr
          | isEmpty expr = empty
          | otherwise = Doc.nest $ text desc $+$ expr $+$ text ""

        srcDoc = exprDoc "-- source" src
        expDoc = exprDoc "-- expanded" exp
        renDoc = exprDoc "-- renamed" ren

definitionError :: String -> String -> Doc
definitionError name err = text name $+$ Doc.nest (text err)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

fromLeft :: Either a b -> a
fromLeft (Left x) = x

docDefn :: Bool -> Bool -> Bool -> Bool -> Definition -> Doc
docDefn showFree showSrc showExp showRen def
  | isNothing (Definition.srcExpr def) =
    definitionError n "definition has no source expression"
  | isNothing (Definition.expExpr def) =
    definitionError n "definition has no expanded expression"
  | isLeft (Definition.renExpr def) =
    definitionError n (fromLeft (Definition.renExpr def))
  | isLeft (Definition.val def) =
    definitionError n (fromLeft (Definition.val def))
  | otherwise =
    definitionOk (Definition.name def)
                     (defVal (Definition.val def))
                     free
                     (srcDoc def)
                     (expDoc def)
                     (renDoc def)
  where n = Definition.name def

        defVal (Right x) = show x

        free
          | showFree = Definition.freeNames def
          | otherwise = []
        
        srcDoc Definition { srcExpr = Just expr }
          | showSrc = Doc.docExpr Doc.SrcDocT expr
          | otherwise = Doc.empty

        expDoc Definition { expExpr = Just expr }
          | showExp = Doc.docExpr Doc.ExpDocT expr
          | otherwise = Doc.empty

        renDoc Definition { renExpr = Right expr }
          | showRen = Doc.docExpr Doc.RenDocT expr
          | otherwise = Doc.empty
