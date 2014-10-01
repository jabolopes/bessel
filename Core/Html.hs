{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, TypeSynonymInstances,
  OverloadedStrings, TupleSections #-}
module Core.Html where

import qualified Text.Blaze.Internal as Blaze (string)
import Text.Blaze.Html5 hiding (head, map)
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html.Renderer.String as Html

import qualified Core as Core
import Data.Module
import Monad.InterpreterM

coreHtmlName :: String
coreHtmlName = "Core.Html"

coreHtmlError :: String -> a
coreHtmlError = error . ((coreHtmlName ++ ".") ++)

-- Tag.

tagType :: Int
IntVal tagType = Core.link $ boxString $ coreHtmlName ++ ".Tag"

isTag :: Val -> Bool
isTag (TypeVal (SeqVal [IntVal typeId, _])) = typeId == tagType
isTag _ = False

mkTag :: (Html -> Html) -> Val
mkTag t = TypeVal (SeqVal [IntVal tagType, dynVal t])

unTag :: Val -> Html -> Html
unTag val@(TypeVal (SeqVal [_, tag]))
  | isTag val =
    let Just tag' = unDynVal tag :: Maybe (Html -> Html) in
    tag'
unTag _ =
  coreHtmlError "unTag: expected tag"

-- Single tag.

singleTagType :: Int
IntVal singleTagType = Core.link $ boxString $ coreHtmlName ++ ".SingleTag"

isSingleTag :: Val -> Bool
isSingleTag (TypeVal (SeqVal [IntVal typeId, _])) = typeId == singleTagType
isSingleTag _ = False

mkSingleTag :: Html -> Val
mkSingleTag t = TypeVal (SeqVal [IntVal singleTagType, dynVal t])

unSingleTag :: Val -> Html
unSingleTag val@(TypeVal (SeqVal [_, tag]))
  | isSingleTag val =
    let Just tag' = unDynVal tag :: Maybe Html in
    tag'
unSingleTag _ =
  coreHtmlError "unSingleTag: expected single tag"

-- Attributes.

applyAttribute :: Val -> Val
applyAttribute tag
  | isTag tag || isSingleTag tag =
    FnVal $ return . applyAttributeHof
  | otherwise =
    coreHtmlError "applyAttribute: expected tag or single tag as first argument"
  where
    applyAttributeHof attr = FnVal $ return . applyAttributeHof'
      where
        applyAttributeHof' val =
          let
            attr' = stringTag . unboxString $ attr
            val' = toValue . unboxString $ val
          in
            if isTag tag then
              mkTag $ unTag tag ! customAttribute attr' val'
            else
              mkSingleTag $ unSingleTag tag ! customAttribute attr' val'

applyTag :: Val -> Val
applyTag tag1
  | isTag tag1 =
    FnVal $ return . applyTagHof
  | otherwise =
    coreHtmlError "applyTag: expected tag as first argument"
  where
    applyTagHof tag2
      | isSingleTag tag2 =
        let
          tag1' = unTag tag1
          tag2' = unSingleTag tag2
        in
         mkSingleTag (tag1' tag2')
      | otherwise =
          coreHtmlError "applyTag: expected single tag as second argument"

applyList :: Val -> Val
applyList tag
  | isTag tag =
    FnVal $ return . applyListHof
  | otherwise =
    coreHtmlError "applyList: expected tag as first argument"
  where
    applyListHof (SeqVal tags)
      | all isSingleTag tags =
        let
          tag' = unTag tag
          tags' = map unSingleTag tags
        in
         mkSingleTag . tag' . toMarkup $ tags'
    applyListHof _ =
      coreHtmlError "applyList: expected list of single tags as second argument"

string :: Val -> Val
string = mkSingleTag . Blaze.string . unboxString

renderHtml :: Val -> Val
renderHtml val
  | isSingleTag val =
    let tag = unSingleTag val in
    boxString $ Html.renderHtml tag
  | otherwise =
    coreHtmlError "renderHtml: expected single tag as first argument"

fnDesc :: FnDesc
fnDesc =
  [("a"           , mkTag Html.a),
   ("abbr"        , mkTag Html.abbr),
   ("address"     , mkTag Html.address),
   ("area"        , mkSingleTag Html.area),
   ("article"     , mkTag Html.article),
   ("aside"       , mkTag Html.aside),
   ("audio"       , mkTag Html.audio),
   ("b"           , mkTag Html.b),
   ("base"        , mkSingleTag Html.base),
   ("bdo"         , mkTag Html.bdo),
   ("blockquote"  , mkTag Html.blockquote),
   ("body"        , mkTag Html.body),
   ("br"          , mkSingleTag Html.br),
   ("button"      , mkTag Html.button),
   ("canvas"      , mkTag Html.canvas),
   ("caption"     , mkTag Html.caption),
   ("cite"        , mkTag Html.cite),
   ("code"        , mkTag Html.code),
   ("col"         , mkSingleTag Html.col),
   ("colgroup"    , mkTag Html.colgroup),
   ("command"     , mkTag Html.command),
   ("datalist"    , mkTag Html.datalist),
   ("dd"          , mkTag Html.dd),
   ("del"         , mkTag Html.del),
   ("details"     , mkTag Html.details),
   ("dfn"         , mkTag Html.dfn),
   -- ("div"      , tag Html.div), -- edit: conflicts with 'div'
   ("dl"          , mkTag Html.dl),
   ("docType"     , mkSingleTag Html.docType),
   ("docTypeHtml" , mkTag Html.docTypeHtml),
   ("dt"          , mkTag Html.dt),
   ("em"          , mkTag Html.em),
   ("embed"       , mkSingleTag Html.embed),
   ("fieldset"    , mkTag Html.fieldset),
   ("figcaption"  , mkTag Html.figcaption),
   ("figure"      , mkTag Html.figure),
   ("footer"      , mkTag Html.footer),
   ("form"        , mkTag Html.form),
   ("h1"          , mkTag Html.h1),
   ("h2"          , mkTag Html.h2),
   ("h3"          , mkTag Html.h3),
   ("h4"          , mkTag Html.h4),
   ("h5"          , mkTag Html.h5),
   ("h6"          , mkTag Html.h6),
   ("head"        , mkTag Html.head),
   ("header"      , mkTag Html.header),
   ("hgroup"      , mkTag Html.hgroup),
   ("hr"          , mkSingleTag Html.hr),
   ("html"        , mkTag Html.html),
   ("i"           , mkTag Html.i),
   ("iframe"      , mkTag Html.iframe),
   ("img"         , mkSingleTag Html.img),
   ("input"       , mkSingleTag Html.input),
   ("ins"         , mkTag Html.ins),
   ("kbd"         , mkTag Html.kbd),
   ("keygen"      , mkSingleTag Html.keygen),
   ("label"       , mkTag Html.label),
   ("legend"      , mkTag Html.legend),
   ("li"          , mkTag Html.li),
   ("link"        , mkSingleTag Html.link),
   ("map"         , mkTag Html.map),
   ("mark"        , mkTag Html.mark),
   ("menu"        , mkTag Html.menu),
   ("meta"        , mkSingleTag Html.meta),
   ("meter"       , mkTag Html.meter),
   ("nav"         , mkTag Html.nav),
   ("noscript"    , mkTag Html.noscript),
   ("object"      , mkTag Html.object),
   ("ol"          , mkTag Html.ol),
   ("optgroup"    , mkTag Html.optgroup),
   ("option"      , mkTag Html.option),
   ("output"      , mkTag Html.output),
   ("p"           , mkTag Html.p),
   ("param"       , mkSingleTag Html.param),
   ("pre"         , mkTag Html.pre),
   ("progress"    , mkTag Html.progress),
   ("q"           , mkTag Html.q),
   ("rp"          , mkTag Html.rp),
   ("rt"          , mkTag Html.rt),
   ("ruby"        , mkTag Html.ruby),
   ("samp"        , mkTag Html.samp),
   ("script"      , mkTag Html.script),
   ("section"     , mkTag Html.section),
   ("select"      , mkTag Html.select),
   ("small"       , mkTag Html.small),
   ("source"      , mkSingleTag Html.source),
   ("span"        , mkTag Html.span),
   ("strong"      , mkTag Html.strong),
   ("style"       , mkTag Html.style),
   -- ("sub"      , tag Html.sub), -- edit: sub is already bound
   ("summary"     , mkTag Html.summary),
   ("sup"         , mkTag Html.sup),
   ("table"       , mkTag Html.table),
   ("tbody"       , mkTag Html.tbody),
   ("td"          , mkTag Html.td),
   ("textarea"    , mkTag Html.textarea),
   ("tfoot"       , mkTag Html.tfoot),
   ("th"          , mkTag Html.th),
   ("thead"       , mkTag Html.thead),
   ("time"        , mkTag Html.time),
   ("title"       , mkTag Html.title),
   ("tr"          , mkTag Html.tr),
   ("ul"          , mkTag Html.ul),
   ("var"         , mkTag Html.var),
   ("video"       , mkTag Html.video),
   ("string"      , primitive string),
   ("applyAttribute", primitive applyAttribute),
   ("applyTag", primitive applyTag),
   ("applyList", primitive applyList),
   ("renderHtml", primitive renderHtml)]

coreHtmlModule :: IO Module
coreHtmlModule = mkCoreModule coreHtmlName ["Core"] fnDesc
