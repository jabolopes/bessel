{-# LANGUAGE DeriveDataTypeable #-}
module Core.Happstack where

import Control.Concurrent (killThread, forkIO)
import Control.Monad
import Data.Dynamic
import qualified Data.Map as Map (fromList)
import Data.Maybe
import System.IO.Unsafe

import Happstack.Server

import Text.Blaze.Html5 hiding (head, map)
import qualified Text.Blaze.Html5 as Html

import Core (m)
import Data.SrcFile
import Data.Type
import Monad.InterpreterM


data ServeCommand
    = ServeDirectory FilePath
      deriving (Typeable)


instance ToMarkup Expr where
    toMarkup (IntExpr i) = toMarkup $ show i
    toMarkup expr@(SeqExpr exprs)
        | not (null exprs) && all isCharExpr exprs = toMarkup $ unboxString expr
        | otherwise = toMarkup $ map toMarkup exprs


instance ToMessage Expr where
    toResponse (IntExpr i) = toResponse $ show i
    toResponse expr@(SeqExpr exprs)
        | not (null exprs) && all isCharExpr exprs = toResponse $ unboxString expr
        | otherwise = toResponse $ show $ map toResponse exprs
    toResponse expr@(DynExpr _) = toResponse (fromDynExpr expr :: Markup)



applyAttributes :: (Html -> Html) -> [Expr] -> Html -> Html
applyAttributes tag [] = tag
applyAttributes tag (TypeExpr _ _ (SeqExpr [attr,val]):attrs) =
    let 
        attr' = stringTag $ unboxString attr
        val' = toValue $ unboxString val
        tag' = tag ! customAttribute attr' val'
    in
      applyAttributes tag' attrs
applyAttributes _ attrs =
    error $ show attrs


link :: [Int] -> (SrcFile, [Int])
link ids = (srcfile, drop 2 ids)
    where tagTypename = "Core.Happstack.Tag"
          tagTid = head ids

          attrTypename = "Core.Happstack.Attribute"
          attrTid = ids !! 1

          attrM :: String -> Expr
          attrM attr =
              FnExpr $ \expr ->
                  return $ TypeExpr attrTypename attrTid $ SeqExpr [boxString attr, expr]

          tagM :: (Html -> Html) -> Expr
          tagM tag =
              FnExpr $ \(SeqExpr attrs) ->
                  return $ FnExpr $ \expr ->
                      return $ toTypeDynExpr tagTypename tagTid $ applyAttributes tag attrs $ toMarkup expr

          singleTagM tag =
              toTypeDynExpr tagTypename tagTid tag

          serve :: Expr -> InterpreterM Expr
          {-# NOINLINE serve #-}
          serve (FnExpr fn) =
              do val <- fn (SeqExpr [])
                 return $ unsafePerformIO $ do
                   threadId <- forkIO $ simpleHTTP nullConf (ok $ toResponse (fromTypeDynExpr tagTid val :: Html))
                   waitForTermination
                   killThread threadId
                   return $ SeqExpr []

          -- edit: fixed undefined
          srcfile :: SrcFile
          srcfile = SrcFile "Core.Happstack" ["Core"] Nothing undefined undefined $ Right
                    ([tagTypename],
                     Map.fromList [("a", (ArrowT DynT DynT, tagM Html.a)),
                                   ("abbr", (ArrowT DynT DynT, tagM Html.abbr)),
                                   ("address", (ArrowT DynT DynT, tagM Html.address)),
                                   -- ("area", (ArrowT DynT DynT, tagM Html.area)),
                                   ("article", (ArrowT DynT DynT, tagM Html.article)),
                                   ("aside", (ArrowT DynT DynT, tagM Html.aside)),
                                   ("audio", (ArrowT DynT DynT, tagM Html.audio)),
                                   ("b", (ArrowT DynT DynT, tagM Html.b)),
                                   ("base", (ArrowT DynT DynT, tagM Html.base)),
                                   ("bdo", (ArrowT DynT DynT, tagM Html.bdo)),
                                   ("blockquote", (ArrowT DynT DynT, tagM Html.blockquote)),
                                   ("body", (ArrowT DynT DynT, tagM Html.body)),
                                   ("br", (ArrowT DynT DynT, singleTagM Html.br)),
                                   ("button", (ArrowT DynT DynT, tagM Html.button)),
                                   ("canvas", (ArrowT DynT DynT, tagM Html.canvas)),
                                   ("caption", (ArrowT DynT DynT, tagM Html.caption)),
                                   ("cite", (ArrowT DynT DynT, tagM Html.cite)),
                                   ("code", (ArrowT DynT DynT, tagM Html.code)),
                                   ("col", (ArrowT DynT DynT, singleTagM Html.col)),
                                   ("colgroup", (ArrowT DynT DynT, tagM Html.colgroup)),
                                   ("command", (ArrowT DynT DynT, tagM Html.command)),
                                   ("datalist", (ArrowT DynT DynT, tagM Html.datalist)),
                                   ("dd", (ArrowT DynT DynT, tagM Html.dd)),
                                   ("del", (ArrowT DynT DynT, tagM Html.del)),
                                   ("details", (ArrowT DynT DynT, tagM Html.details)),
                                   ("dfn", (ArrowT DynT DynT, tagM Html.dfn)),
                                   -- ("div", (ArrowT DynT DynT, tagM Html.div)), -- edit: conflicts with 'div'
                                   ("dl", (ArrowT DynT DynT, tagM Html.dl)),
                                   -- ("docType", (ArrowT DynT DynT, Html.docType)),
                                   ("docTypeHtml", (ArrowT DynT DynT, tagM Html.docTypeHtml)),
                                   ("dt", (ArrowT DynT DynT, tagM Html.dt)),
                                   ("em", (ArrowT DynT DynT, tagM Html.em)),
                                   ("embed", (ArrowT DynT DynT, singleTagM Html.embed)),
                                   ("fieldset", (ArrowT DynT DynT, tagM Html.fieldset)),
                                   ("figcaption", (ArrowT DynT DynT, tagM Html.figcaption)),
                                   ("figure", (ArrowT DynT DynT, tagM Html.figure)),
                                   ("footer", (ArrowT DynT DynT, tagM Html.footer)),
                                   ("form", (ArrowT DynT DynT, tagM Html.form)),
                                   ("h1", (ArrowT DynT DynT, tagM Html.h1)),
                                   ("h2", (ArrowT DynT DynT, tagM Html.h2)),
                                   ("h3", (ArrowT DynT DynT, tagM Html.h3)),
                                   ("h4", (ArrowT DynT DynT, tagM Html.h4)),
                                   ("h5", (ArrowT DynT DynT, tagM Html.h5)),
                                   ("h6", (ArrowT DynT DynT, tagM Html.h6)),
                                   ("head", (ArrowT DynT DynT, tagM Html.head)),
                                   ("header", (ArrowT DynT DynT, tagM Html.header)),
                                   ("hgroup", (ArrowT DynT DynT, tagM Html.hgroup)),
                                   ("hr", (ArrowT DynT DynT, singleTagM Html.hr)),
                                   ("html", (ArrowT DynT DynT, tagM Html.html)),
                                   ("i", (ArrowT DynT DynT, tagM Html.i)),
                                   ("iframe", (ArrowT DynT DynT, tagM Html.iframe)),
                                   ("img", (ArrowT DynT DynT, singleTagM Html.img)),
                                   ("input", (ArrowT DynT DynT, singleTagM Html.input)),
                                   ("ins", (ArrowT DynT DynT, tagM Html.ins)),
                                   ("kbd", (ArrowT DynT DynT, tagM Html.kbd)),
                                   ("keygen", (ArrowT DynT DynT, tagM Html.keygen)),
                                   ("label", (ArrowT DynT DynT, tagM Html.label)),
                                   ("legend", (ArrowT DynT DynT, tagM Html.legend)),
                                   ("li", (ArrowT DynT DynT, tagM Html.li)),
                                   ("link", (ArrowT DynT DynT, singleTagM Html.link)),
                                   ("map", (ArrowT DynT DynT, tagM Html.map)),
                                   ("mark", (ArrowT DynT DynT, tagM Html.mark)),
                                   ("menu", (ArrowT DynT DynT, tagM Html.menu)),
                                   ("meta", (ArrowT DynT DynT, singleTagM Html.meta)),
                                   ("meter", (ArrowT DynT DynT, tagM Html.meter)),
                                   ("nav", (ArrowT DynT DynT, tagM Html.nav)),
                                   ("noscript", (ArrowT DynT DynT, tagM Html.noscript)),
                                   ("object", (ArrowT DynT DynT, tagM Html.object)),
                                   ("ol", (ArrowT DynT DynT, tagM Html.ol)),
                                   ("optgroup", (ArrowT DynT DynT, tagM Html.optgroup)),
                                   ("option", (ArrowT DynT DynT, tagM Html.option)),
                                   ("output", (ArrowT DynT DynT, tagM Html.output)),
                                   ("p", (ArrowT DynT DynT, tagM Html.p)),
                                   ("param", (ArrowT DynT DynT, singleTagM Html.param)),
                                   ("pre", (ArrowT DynT DynT, tagM Html.pre)),
                                   ("progress", (ArrowT DynT DynT, tagM Html.progress)),
                                   ("q", (ArrowT DynT DynT, tagM Html.q)),
                                   ("rp", (ArrowT DynT DynT, tagM Html.rp)),
                                   ("rt", (ArrowT DynT DynT, tagM Html.rt)),
                                   ("ruby", (ArrowT DynT DynT, tagM Html.ruby)),
                                   ("samp", (ArrowT DynT DynT, tagM Html.samp)),
                                   ("script", (ArrowT DynT DynT, tagM Html.script)),
                                   ("section", (ArrowT DynT DynT, tagM Html.section)),
                                   ("select", (ArrowT DynT DynT, tagM Html.select)),
                                   ("small", (ArrowT DynT DynT, tagM Html.small)),
                                   ("source", (ArrowT DynT DynT, tagM Html.source)),
                                   ("span", (ArrowT DynT DynT, tagM Html.span)),
                                   ("strong", (ArrowT DynT DynT, tagM Html.strong)),
                                   ("style", (ArrowT DynT DynT, tagM Html.style)),
                                   -- ("sub", (ArrowT DynT DynT, tagM Html.sub)), -- edit: sub is already bound
                                   ("summary", (ArrowT DynT DynT, tagM Html.summary)),
                                   ("sup", (ArrowT DynT DynT, tagM Html.sup)),
                                   ("table", (ArrowT DynT DynT, tagM Html.table)),
                                   ("tbody", (ArrowT DynT DynT, tagM Html.tbody)),
                                   ("td", (ArrowT DynT DynT, tagM Html.td)),
                                   ("textarea", (ArrowT DynT DynT, tagM Html.textarea)),
                                   ("tfoot", (ArrowT DynT DynT, tagM Html.tfoot)),
                                   ("th", (ArrowT DynT DynT, tagM Html.th)),
                                   ("thead", (ArrowT DynT DynT, tagM Html.thead)),
                                   ("time", (ArrowT DynT DynT, tagM Html.time)),
                                   ("title", (ArrowT DynT DynT, tagM Html.title)),
                                   ("tr", (ArrowT DynT DynT, tagM Html.tr)),
                                   ("ul", (ArrowT DynT DynT, tagM Html.ul)),
                                   ("var", (ArrowT DynT DynT, tagM Html.var)),
                                   ("video", (ArrowT DynT DynT, tagM Html.video)),

                                   ("class", (ArrowT DynT DynT, attrM "class")),
                                   ("href", (ArrowT DynT DynT, attrM "href")),
                                   ("httpEquiv", (ArrowT DynT DynT, attrM "httpEquiv")),
                                   ("name", (ArrowT DynT DynT, attrM "name")),
                                   ("rel", (ArrowT DynT DynT, attrM "rel")),
                                   ("src", (ArrowT DynT DynT, attrM "src")),
                                   ("style", (ArrowT DynT DynT, attrM "style")),
                                   ("type_", (ArrowT DynT DynT, attrM "type_")),
                                   ("width", (ArrowT DynT DynT, attrM "width")),

                                   ("serve", (ArrowT (SeqT CharT) DynT, FnExpr serve))])