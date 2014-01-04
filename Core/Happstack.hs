{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, TypeSynonymInstances #-}
module Core.Happstack where

import Data.Typeable (Typeable)

import Happstack.Lite (ServerPart, Response, Browsing(..), msum)
import qualified Happstack.Lite as Happstack
import qualified Happstack.Server as Server (uriRest)

import qualified Text.Blaze.Internal as Blaze (string)
import Text.Blaze.Html5 hiding (head, map)
import qualified Text.Blaze.Html5 as Html

import qualified Core as Core
import Data.Module
import Monad.InterpreterM

happstackName :: String
happstackName = "Core.Happstack"

happstackError :: String -> a
happstackError = error . ((happstackName ++ ".") ++)

tagType :: Int
IntVal tagType = Core.link $ boxString $ happstackName ++ ".Tag"

isTag :: Val -> Bool
isTag (TypeVal (SeqVal [IntVal typeId, _])) = typeId == tagType
isTag _ = False

mkTag :: (Html -> Html) -> Val
mkTag t = TypeVal (SeqVal [IntVal tagType, dynVal t])

unTag :: Val -> Html -> Html
unTag val@(TypeVal (SeqVal [_, tag]))
  | isTag  val =
    let Just tag' = unDynVal tag :: Maybe (Html -> Html) in
    tag'
unTag _ =
  happstackError "unTag: expected tag"

singleTagType :: Int
IntVal singleTagType = Core.link $ boxString $ happstackName ++ ".SingleTag"

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
  happstackError "unTag: expected single tag"

applyAttribute :: Val -> Val
applyAttribute tag
  | isTag tag || isSingleTag tag = FnVal hof
  | otherwise =
    happstackError "applyAttribute: expected tag or single tag as first argument"
  where
    hof attr = FnVal hof'
      where
        apply tag attr val =
          tag ! customAttribute attr val

        hof' val =
          let
            attr' = stringTag . unboxString $ attr
            val' = toValue . unboxString $ val
          in
            if isTag tag then
              mkTag $ apply (unTag tag) attr' val'
            else
              mkSingleTag $ apply (unSingleTag tag) attr' val'

applyTag :: Val -> Val
applyTag tag1
  | isTag tag1 = FnVal hof
  | otherwise = 
    happstackError "applyTag: expected tag as first argument"
  where hof tag2
          | isSingleTag tag2 =
            let
              tag1' = unTag tag1
              tag2' = unSingleTag tag2
            in
              mkSingleTag (tag1' tag2')
          | otherwise =
            happstackError "applyTag: expected single tag as second argument"

applyList :: Val -> Val
applyList tag
  | isTag tag = FnVal hof
  | otherwise =
    happstackError "applyList: expected tag as first argument"
  where hof (SeqVal tags)
          | all isSingleTag tags =
            let
              tag' = unTag tag
              tags' = map unSingleTag tags
            in
              mkSingleTag . tag' . toMarkup $ tags'
        hof _ =
          happstackError "applyList: expected list of single tags as second argument"

string :: Val -> Val
string = mkSingleTag . Blaze.string . unboxString

-- server stuff

class MonadVal a where
  runMonadVal :: Val -> a

data Serve
  = DirectoryServe String
    deriving (Typeable)

serveType :: Int
IntVal serveType = Core.link $ boxString $ happstackName ++ ".Serve"

isServe :: Val -> Bool
isServe (TypeVal (SeqVal [IntVal typeId, _])) = typeId == serveType
isServe _ = False

mkServe :: Serve -> Val
mkServe t = TypeVal (SeqVal [IntVal serveType, dynVal t])

unServe :: Val -> Serve
unServe val@(TypeVal (SeqVal [_, serve]))
  | isServe  val =
    let Just serve' = unDynVal serve :: Maybe Serve in
    serve'
unServe _ =
  happstackError "unServe: expected Serve"

instance MonadVal (ServerPart Response) where
  runMonadVal val
    | isServe val =
      case unServe val of
        DirectoryServe x ->
          Happstack.serveDirectory DisableBrowsing ["index.html"] x
    | otherwise =
      happstackError $ "MonadVal.runMonadVal: expected Serve as first argument"

serveDirectory :: Val -> Val
serveDirectory val
  | isStringVal val =
    mkServe . DirectoryServe . unboxString $ val
  | otherwise =
    happstackError $ "serveDirectory: expected string as first argument"

serveApp :: Val -> Val
serveApp (FnVal fn) =
  IOVal $ do
    Happstack.serve Nothing $
      msum [ Server.uriRest (runMonadVal . fn . boxString)
           ]
    return $ SeqVal []
serveApp _ =
  happstackError "serveApp: expected (String -> Serve) as first argument"

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

   ("serveApp"    , FnVal serveApp),
   ("serveDirectory" , FnVal serveDirectory),
   ("string"      , FnVal string),
   ("applyAttribute", FnVal applyAttribute),
   ("applyTag", FnVal applyTag),
   ("applyList", FnVal applyList)]

happstackModule :: Module
happstackModule = mkCoreModule happstackName ["Core"] fnDesc
