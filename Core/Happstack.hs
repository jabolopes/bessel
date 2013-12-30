{-# LANGUAGE DeriveDataTypeable #-}
module Core.Happstack where

import Control.Concurrent (killThread, forkIO)
import qualified Data.ByteString.Lazy.Char8 as LazyByteString (unpack)
import qualified Data.ByteString.Char8 as StrictByteString (pack)
import System.IO.Unsafe

import Happstack.Server

import Text.Blaze.Internal (Attributable)
import qualified Text.Blaze.Internal as Blaze (string)
import Text.Blaze.Html5 hiding (head, map)
import qualified Text.Blaze.Html5 as Html
import Text.Blaze.Renderer.Utf8 (renderMarkup)

import qualified Core
import Data.Module
import Monad.InterpreterM

happstackName :: String
happstackName = "Core.Happstack"

tagTypename :: String
tagTypename = happstackName ++ ".Tag"

tagTid :: Val
tagTid = Core.link (boxString tagTypename)

attrTypename :: String
attrTypename = happstackName ++ ".Attribute"

attrTid :: Val
attrTid = Core.link (boxString attrTypename)

attrM :: String -> Val
attrM attr = dynVal attr

tagM :: (Html -> Html) -> Val
tagM tag = dynVal tag

applyAttribute :: Val -> Val
applyAttribute tag@DynVal {} = FnVal hof
  where Just tag' = unDynVal tag :: Maybe Html

        hof attr = FnVal hof'
          where attr' = stringTag (unboxString attr)

                hof' val =
                  let val' = toValue (unboxString val) in
                  dynVal (tag' ! customAttribute attr' val')

applyTag :: Val -> Val
applyTag tag1@DynVal {} =
  let Just tag1' = unDynVal tag1 :: Maybe (Html -> Html) in
  FnVal (hof tag1')
  where hof tag1' tag2@DynVal {} =
          let Just tag2' = unDynVal tag2 :: Maybe Html in
          dynVal (tag1' tag2')

string :: Val -> Val
string = dynVal . Blaze.string . unboxString

singleTagM :: Html -> Val
singleTagM = undefined

{-# NOINLINE serve #-}
serve :: Val -> Val
serve val@DynVal {} =
  let Just val' = unDynVal val :: Maybe Html in
  unsafePerformIO $ do
    threadId <- forkIO $ simpleHTTP nullConf (ok . toResponse $ val')
    waitForTermination
    killThread threadId
    return $ SeqVal []
serve _ = error $ happstackName ++ ".serve: expected HTML value as first argument"

fnDesc :: FnDesc
fnDesc =
  [("a"           , tagM Html.a),
   ("abbr"        , tagM Html.abbr),
   ("address"     , tagM Html.address),
   ("area"        , singleTagM Html.area),
   ("article"     , tagM Html.article),
   ("aside"       , tagM Html.aside),
   ("audio"       , tagM Html.audio),
   ("b"           , tagM Html.b),
   ("base"        , singleTagM Html.base),
   ("bdo"         , tagM Html.bdo),
   ("blockquote"  , tagM Html.blockquote),
   ("body"        , tagM Html.body),
   ("br"          , singleTagM Html.br),
   ("button"      , tagM Html.button),
   ("canvas"      , tagM Html.canvas),
   ("caption"     , tagM Html.caption),
   ("cite"        , tagM Html.cite),
   ("code"        , tagM Html.code),
   ("col"         , singleTagM Html.col),
   ("colgroup"    , tagM Html.colgroup),
   ("command"     , tagM Html.command),
   ("datalist"    , tagM Html.datalist),
   ("dd"          , tagM Html.dd),
   ("del"         , tagM Html.del),
   ("details"     , tagM Html.details),
   ("dfn"         , tagM Html.dfn),
   -- ("div"      , tagM Html.div), -- edit: conflicts with 'div'
   ("dl"          , tagM Html.dl),
   ("docType"     , singleTagM Html.docType),
   ("docTypeHtml" , tagM Html.docTypeHtml),
   ("dt"          , tagM Html.dt),
   ("em"          , tagM Html.em),
   ("embed"       , singleTagM Html.embed),
   ("fieldset"    , tagM Html.fieldset),
   ("figcaption"  , tagM Html.figcaption),
   ("figure"      , tagM Html.figure),
   ("footer"      , tagM Html.footer),
   ("form"        , tagM Html.form),
   ("h1"          , tagM Html.h1),
   ("h2"          , tagM Html.h2),
   ("h3"          , tagM Html.h3),
   ("h4"          , tagM Html.h4),
   ("h5"          , tagM Html.h5),
   ("h6"          , tagM Html.h6),
   ("head"        , tagM Html.head),
   ("header"      , tagM Html.header),
   ("hgroup"      , tagM Html.hgroup),
   ("hr"          , singleTagM Html.hr),
   ("html"        , tagM Html.html),
   ("i"           , tagM Html.i),
   ("iframe"      , tagM Html.iframe),
   ("img"         , singleTagM Html.img),
   ("input"       , singleTagM Html.input),
   ("ins"         , tagM Html.ins),
   ("kbd"         , tagM Html.kbd),
   ("keygen"      , singleTagM Html.keygen),
   ("label"       , tagM Html.label),
   ("legend"      , tagM Html.legend),
   ("li"          , tagM Html.li),
   ("link"        , singleTagM Html.link),
   ("map"         , tagM Html.map),
   ("mark"        , tagM Html.mark),
   ("menu"        , tagM Html.menu),
   ("meta"        , singleTagM Html.meta),
   ("meter"       , tagM Html.meter),
   ("nav"         , tagM Html.nav),
   ("noscript"    , tagM Html.noscript),
   ("object"      , tagM Html.object),
   ("ol"          , tagM Html.ol),
   ("optgroup"    , tagM Html.optgroup),
   ("option"      , tagM Html.option),
   ("output"      , tagM Html.output),
   ("p"           , tagM Html.p),
   ("param"       , singleTagM Html.param),
   ("pre"         , tagM Html.pre),
   ("progress"    , tagM Html.progress),
   ("q"           , tagM Html.q),
   ("rp"          , tagM Html.rp),
   ("rt"          , tagM Html.rt),
   ("ruby"        , tagM Html.ruby),
   ("samp"        , tagM Html.samp),
   ("script"      , tagM Html.script),
   ("section"     , tagM Html.section),
   ("select"      , tagM Html.select),
   ("small"       , tagM Html.small),
   ("source"      , singleTagM Html.source),
   ("span"        , tagM Html.span),
   ("strong"      , tagM Html.strong),
   ("style"       , tagM Html.style),
   -- ("sub"      , tagM Html.sub), -- edit: sub is already bound
   ("summary"     , tagM Html.summary),
   ("sup"         , tagM Html.sup),
   ("table"       , tagM Html.table),
   ("tbody"       , tagM Html.tbody),
   ("td"          , tagM Html.td),
   ("textarea"    , tagM Html.textarea),
   ("tfoot"       , tagM Html.tfoot),
   ("th"          , tagM Html.th),
   ("thead"       , tagM Html.thead),
   ("time"        , tagM Html.time),
   ("title"       , tagM Html.title),
   ("tr"          , tagM Html.tr),
   ("ul"          , tagM Html.ul),
   ("var"         , tagM Html.var),
   ("video"       , tagM Html.video),

   ("class"       , attrM "class"),
   ("href"        , attrM "href"),
   ("httpEquiv"   , attrM "httpEquiv"),
   ("name"        , attrM "name"),
   ("rel"         , attrM "rel"),
   ("src"         , attrM "src"),
   ("style"       , attrM "style"),
   ("type_"       , attrM "type_"),
   ("width"       , attrM "width"),

   ("serve"       , FnVal serve),
   ("string"      , FnVal string),
   ("applyAttribute", FnVal applyAttribute),
   ("applyTag", FnVal applyTag)]

  -- [("a", (ArrowT DynT DynT, tagM Html.a)),
  --  ("abbr", (ArrowT DynT DynT, tagM Html.abbr)),
  --  ("address", (ArrowT DynT DynT, tagM Html.address)),
  --  -- ("area", (ArrowT DynT DynT, tagM Html.area)),
  --  ("article", (ArrowT DynT DynT, tagM Html.article)),
  --  ("aside", (ArrowT DynT DynT, tagM Html.aside)),
  --  ("audio", (ArrowT DynT DynT, tagM Html.audio)),
  --  ("b", (ArrowT DynT DynT, tagM Html.b)),
  --  ("base", (ArrowT DynT DynT, tagM Html.base)),
  --  ("bdo", (ArrowT DynT DynT, tagM Html.bdo)),
  --  ("blockquote", (ArrowT DynT DynT, tagM Html.blockquote)),
  --  ("body", (ArrowT DynT DynT, tagM Html.body)),
  --  ("br", (ArrowT DynT DynT, singleTagM Html.br)),
  --  ("button", (ArrowT DynT DynT, tagM Html.button)),
  --  ("canvas", (ArrowT DynT DynT, tagM Html.canvas)),
  --  ("caption", (ArrowT DynT DynT, tagM Html.caption)),
  --  ("cite", (ArrowT DynT DynT, tagM Html.cite)),
  --  ("code", (ArrowT DynT DynT, tagM Html.code)),
  --  ("col", (ArrowT DynT DynT, singleTagM Html.col)),
  --  ("colgroup", (ArrowT DynT DynT, tagM Html.colgroup)),
  --  ("command", (ArrowT DynT DynT, tagM Html.command)),
  --  ("datalist", (ArrowT DynT DynT, tagM Html.datalist)),
  --  ("dd", (ArrowT DynT DynT, tagM Html.dd)),
  --  ("del", (ArrowT DynT DynT, tagM Html.del)),
  --  ("details", (ArrowT DynT DynT, tagM Html.details)),
  --  ("dfn", (ArrowT DynT DynT, tagM Html.dfn)),
  --  -- ("div", (ArrowT DynT DynT, tagM Html.div)), -- edit: conflicts with 'div'
  --  ("dl", (ArrowT DynT DynT, tagM Html.dl)),
  --  -- ("docType", (ArrowT DynT DynT, Html.docType)),
  --  ("docTypeHtml", (ArrowT DynT DynT, tagM Html.docTypeHtml)),
  --  ("dt", (ArrowT DynT DynT, tagM Html.dt)),
  --  ("em", (ArrowT DynT DynT, tagM Html.em)),
  --  ("embed", (ArrowT DynT DynT, singleTagM Html.embed)),
  --  ("fieldset", (ArrowT DynT DynT, tagM Html.fieldset)),
  --  ("figcaption", (ArrowT DynT DynT, tagM Html.figcaption)),
  --  ("figure", (ArrowT DynT DynT, tagM Html.figure)),
  --  ("footer", (ArrowT DynT DynT, tagM Html.footer)),
  --  ("form", (ArrowT DynT DynT, tagM Html.form)),
  --  ("h1", (ArrowT DynT DynT, tagM Html.h1)),
  --  ("h2", (ArrowT DynT DynT, tagM Html.h2)),
  --  ("h3", (ArrowT DynT DynT, tagM Html.h3)),
  --  ("h4", (ArrowT DynT DynT, tagM Html.h4)),
  --  ("h5", (ArrowT DynT DynT, tagM Html.h5)),
  --  ("h6", (ArrowT DynT DynT, tagM Html.h6)),
  --  ("head", (ArrowT DynT DynT, tagM Html.head)),
  --  ("header", (ArrowT DynT DynT, tagM Html.header)),
  --  ("hgroup", (ArrowT DynT DynT, tagM Html.hgroup)),
  --  ("hr", (ArrowT DynT DynT, singleTagM Html.hr)),
  --  ("html", (ArrowT DynT DynT, tagM Html.html)),
  --  ("i", (ArrowT DynT DynT, tagM Html.i)),
  --  ("iframe", (ArrowT DynT DynT, tagM Html.iframe)),
  --  ("img", (ArrowT DynT DynT, singleTagM Html.img)),
  --  ("input", (ArrowT DynT DynT, singleTagM Html.input)),
  --  ("ins", (ArrowT DynT DynT, tagM Html.ins)),
  --  ("kbd", (ArrowT DynT DynT, tagM Html.kbd)),
  --  ("keygen", (ArrowT DynT DynT, tagM Html.keygen)),
  --  ("label", (ArrowT DynT DynT, tagM Html.label)),
  --  ("legend", (ArrowT DynT DynT, tagM Html.legend)),
  --  ("li", (ArrowT DynT DynT, tagM Html.li)),
  --  ("link", (ArrowT DynT DynT, singleTagM Html.link)),
  --  ("map", (ArrowT DynT DynT, tagM Html.map)),
  --  ("mark", (ArrowT DynT DynT, tagM Html.mark)),
  --  ("menu", (ArrowT DynT DynT, tagM Html.menu)),
  --  ("meta", (ArrowT DynT DynT, singleTagM Html.meta)),
  --  ("meter", (ArrowT DynT DynT, tagM Html.meter)),
  --  ("nav", (ArrowT DynT DynT, tagM Html.nav)),
  --  ("noscript", (ArrowT DynT DynT, tagM Html.noscript)),
  --  ("object", (ArrowT DynT DynT, tagM Html.object)),
  --  ("ol", (ArrowT DynT DynT, tagM Html.ol)),
  --  ("optgroup", (ArrowT DynT DynT, tagM Html.optgroup)),
  --  ("option", (ArrowT DynT DynT, tagM Html.option)),
  --  ("output", (ArrowT DynT DynT, tagM Html.output)),
  --  ("p", (ArrowT DynT DynT, tagM Html.p)),
  --  ("param", (ArrowT DynT DynT, singleTagM Html.param)),
  --  ("pre", (ArrowT DynT DynT, tagM Html.pre)),
  --  ("progress", (ArrowT DynT DynT, tagM Html.progress)),
  --  ("q", (ArrowT DynT DynT, tagM Html.q)),
  --  ("rp", (ArrowT DynT DynT, tagM Html.rp)),
  --  ("rt", (ArrowT DynT DynT, tagM Html.rt)),
  --  ("ruby", (ArrowT DynT DynT, tagM Html.ruby)),
  --  ("samp", (ArrowT DynT DynT, tagM Html.samp)),
  --  ("script", (ArrowT DynT DynT, tagM Html.script)),
  --  ("section", (ArrowT DynT DynT, tagM Html.section)),
  --  ("select", (ArrowT DynT DynT, tagM Html.select)),
  --  ("small", (ArrowT DynT DynT, tagM Html.small)),
  --  ("source", (ArrowT DynT DynT, tagM Html.source)),
  --  ("span", (ArrowT DynT DynT, tagM Html.span)),
  --  ("strong", (ArrowT DynT DynT, tagM Html.strong)),
  --  ("style", (ArrowT DynT DynT, tagM Html.style)),
  --  -- ("sub", (ArrowT DynT DynT, tagM Html.sub)), -- edit: sub is already bound
  --  ("summary", (ArrowT DynT DynT, tagM Html.summary)),
  --  ("sup", (ArrowT DynT DynT, tagM Html.sup)),
  --  ("table", (ArrowT DynT DynT, tagM Html.table)),
  --  ("tbody", (ArrowT DynT DynT, tagM Html.tbody)),
  --  ("td", (ArrowT DynT DynT, tagM Html.td)),
  --  ("textarea", (ArrowT DynT DynT, tagM Html.textarea)),
  --  ("tfoot", (ArrowT DynT DynT, tagM Html.tfoot)),
  --  ("th", (ArrowT DynT DynT, tagM Html.th)),
  --  ("thead", (ArrowT DynT DynT, tagM Html.thead)),
  --  ("time", (ArrowT DynT DynT, tagM Html.time)),
  --  ("title", (ArrowT DynT DynT, tagM Html.title)),
  --  ("tr", (ArrowT DynT DynT, tagM Html.tr)),
  --  ("ul", (ArrowT DynT DynT, tagM Html.ul)),
  --  ("var", (ArrowT DynT DynT, tagM Html.var)),
  --  ("video", (ArrowT DynT DynT, tagM Html.video)),

  --  ("class", (ArrowT DynT DynT, attrM "class")),
  --  ("href", (ArrowT DynT DynT, attrM "href")),
  --  ("httpEquiv", (ArrowT DynT DynT, attrM "httpEquiv")),
  --  ("name", (ArrowT DynT DynT, attrM "name")),
  --  ("rel", (ArrowT DynT DynT, attrM "rel")),
  --  ("src", (ArrowT DynT DynT, attrM "src")),
  --  ("style", (ArrowT DynT DynT, attrM "style")),
  --  ("type_", (ArrowT DynT DynT, attrM "type_")),
  --  ("width", (ArrowT DynT DynT, attrM "width")),

  --  ("serve", (ArrowT (SeqT CharT) DynT, FnVal serve))])

happstackModule :: Module
happstackModule = mkCoreModule happstackName ["Core"] fnDesc
