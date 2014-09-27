{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, TypeSynonymInstances,
  OverloadedStrings, TupleSections #-}
module Core.Happstack where

import Control.Applicative ((<$>))
import Data.Typeable (Typeable)
import qualified System.Directory as Directory
import Control.Monad.IO.Class (liftIO)
import System.FilePath ((</>))

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteStringChar8

import System.IO
import Control.Exception

import qualified Data.List as List (find, lookup)

import qualified Network.HaskellNet.IMAP as IMAP
import qualified Network.HaskellNet.IMAP.SSL as IMAP

import Data.MBox (Message)
import qualified Data.MBox as MBox

import qualified Data.Text as Text
import qualified Data.Text.IO as Text (readFile)

import Happstack.Lite (ServerPart, Response, Browsing(..), msum)
import qualified Happstack.Lite as Happstack
import qualified Happstack.Server as Server (uriRest)

import Happstack.Lite (Method(..), ok, method)

import qualified Text.Blaze.Internal as Blaze (string)
import Text.Blaze.Html5 hiding (head, map)
import qualified Text.Blaze.Html5.Attributes as A
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
  | isTag tag || isSingleTag tag = FnVal $ return . hof
  | otherwise = happstackError "applyAttribute: expected tag or single tag as first argument"
  where
    hof attr = FnVal $ return . hof'
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
  | isTag tag1 = FnVal $ return . hof
  | otherwise =  happstackError "applyTag: expected tag as first argument"
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
  | isTag tag = FnVal $ return . hof
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

mailBox :: FilePath
mailBox = "/home/jose/Projects/bessel/dist/data/mail"

mailSpool :: FilePath
mailSpool = "/var/spool/mail/jose"

storageDir :: FilePath
storageDir = "/home/jose/Projects/bessel/dist/data/storage"

serveApp :: Val -> InterpreterM Val
serveApp (FnVal fn) =
  return .
  IOVal $ do
    Happstack.serve Nothing $
      msum [ imap
           -- , mail mailBox mailSpool
           -- , storage storageDir
           -- , upload storageDir
           , Happstack.dir "web" $ Server.uriRest (\uri -> do val <- fn $ boxString uri
                                                           runMonadVal <$> fn (boxString uri))
           , top
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

   ("serveApp"    , primitive serveApp),
   ("serveDirectory" , primitive serveDirectory),
   ("string"      , primitive string),
   ("applyAttribute", primitive applyAttribute),
   ("applyTag", primitive applyTag),
   ("applyList", primitive applyList)]

happstackModule :: Module
happstackModule = mkCoreModule happstackName ["Core"] fnDesc

template :: String -> Html -> Response
template title body = Happstack.toResponse $
  Html.html $ do
    Html.head $ do
      Html.title (toHtml title)
    Html.body $ do
      body
      Html.p $ Html.a ! A.href "/" $ "back home"

top :: ServerPart Response
top =
  ok $ template "Top" $ do
    Html.p $ Html.a ! A.href "web" $ "Web"
    Html.p $ Html.a ! A.href "mail" $ "Mail"
    Html.p $ Html.a ! A.href "storage" $ "Storage"
    Html.p $ Html.a ! A.href "upload" $ "Upload"

upload :: FilePath -> ServerPart Response
upload dir =
  Happstack.dir "upload" $
  msum [ uploadForm
       , handleUpload
       ]
  where
    uploadForm :: ServerPart Response
    uploadForm =
        do method GET
           ok $ template "upload form" $ do
             Html.form ! A.enctype "multipart/form-data" ! A.method "POST" ! A.action "/upload" $ do
               Html.input ! A.type_ "file" ! A.name "file_upload" ! A.size "40"
               Html.input ! A.type_ "submit" ! A.value "upload"

    handleUpload :: ServerPart Response
    handleUpload =
        do (tmpFile, uploadName, contentType) <- Happstack.lookFile "file_upload"
           liftIO $ Directory.renameFile tmpFile (dir </> uploadName)
           ok $ template "file uploaded" $ do
                p (toHtml $ "temporary file: " ++ tmpFile)
                p (toHtml $ "uploaded name:  " ++ uploadName)
                p (toHtml $ "content-type:   " ++ show contentType)
                p (toHtml $ "permanent file: " ++ (dir </> uploadName))

-- getHeader :: Message -> String -> Maybe String
-- getHeader msg hd = show . snd <$> (List.find headerFn . MBox.headers $ msg)
--   where headerFn (x, _)
--           | x == Text.pack hd = True
--           | otherwise = False

-- messageHtml :: Message -> Maybe Html
-- messageHtml msg =
--   do to <- getHeader msg "To"
--      from <- getHeader msg "From"
--      subject <- getHeader msg "Subject"
--      date <- getHeader msg "Date"
--      let body = Text.unpack $ MBox.body msg
--      return $ do
--        Html.p $ do
--          toHtml to
--          Html.br
--          toHtml from
--          Html.br
--          toHtml subject
--          Html.br
--          toHtml date
--          Html.br
--          Html.br
--          toHtml body

-- maybeMessageHtml :: Message -> Html
-- maybeMessageHtml msg =
--   case messageHtml msg of
--     Nothing -> toHtml . map (Html.p . toHtml . Text.unpack . fst) . MBox.headers $ msg
--     Just x -> x

-- mail :: FilePath -> FilePath -> ServerPart Response
-- mail mailbox mailspool =
--   Happstack.dir "mail" $ do
--     liftIO $ do
--       str <- readFile mailspool
--       appendFile mailbox str
--       writeFile mailspool ""
--     str <- liftIO $ do
--       txt <- Text.readFile mailbox
--       let mbox = MBox.parseMBox txt
--       return $ map maybeMessageHtml mbox
--     ok $ template "mail" $ do
--       toHtml str

storage :: FilePath -> ServerPart Response
storage dir =
  Happstack.dir "storage" $
    Happstack.serveDirectory EnableBrowsing [] dir

imapServer :: String
imapServer = "imap-mail.outlook.com"

imapUsername :: String
imapUsername = "jabolopes@live.com.pt"

imapPassword :: String
imapPassword = "movepola123"

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

getPassword :: IO String
getPassword = do
  putStr "Password: "
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

breakSubstring :: ByteString -> ByteString -> (ByteString, ByteString)
breakSubstring pat str =
  case ByteString.breakSubstring pat str of
    (x, y) | ByteString.isPrefixOf pat y ->
      (x,) $ ByteString.drop (ByteString.length pat) y
    x -> x

parseImapMessage :: ByteString -> Maybe (ByteString, ByteString)
parseImapMessage msg =
  case ByteString.breakSubstring unixNewline (normalizeMsg msg) of
    (_, "") -> Nothing
    (hds, env) | ByteString.isPrefixOf unixNewline env ->
      Just . (hds,) $ ByteString.drop (ByteString.length unixNewline) env
    (hds, env) ->
      Just (hds, env)
  where unixNewline = ByteStringChar8.pack "\n\n"
        normalizeMsg = ByteStringChar8.filter (/= '\r')

headerImapMessage :: String -> ByteString -> Maybe String
headerImapMessage name hds =
  ByteStringChar8.unpack <$>
    (List.lookup (ByteStringChar8.pack name) .
      map (breakSubstring ":") .
        ByteStringChar8.lines $ hds)

imapMessageHtml :: ByteString -> Maybe Html
imapMessageHtml msg =
  do (hds, env) <- parseImapMessage msg
     to <- headerImapMessage "To" hds
     from <- headerImapMessage "From" hds
     subject <- headerImapMessage "Subject" hds
     date <- headerImapMessage "Date" hds
     let body = Html.contents $ Html.unsafeByteString env
     return $ do
       Html.p $ do
         toHtml to
         Html.br
         toHtml from
         Html.br
         toHtml subject
         Html.br
         toHtml date
         Html.br
         Html.br
         body

maybeImapMessageHtml :: ByteString -> Html
maybeImapMessageHtml msg =
  case imapMessageHtml msg of
    Nothing -> Html.p . toHtml . ByteStringChar8.unpack $ msg
    Just x -> x

mailImap :: IO [ByteString]
mailImap = do
  con <- IMAP.connectIMAPSSL imapServer

  IMAP.login con imapUsername imapPassword
  -- mboxes <- IMAP.list con

  IMAP.examine con "INBOX"
  msgs <- IMAP.search con [IMAP.ALLs]

  mapM (IMAP.fetch con) msgs

imap :: ServerPart Response
imap =
  Happstack.dir "imap" $ do
    msgs <- liftIO mailImap
    ok $ template "imap" $
      toHtml $ map maybeImapMessageHtml msgs

-- Happstack.dir "storage" list
--   where
--     blacklist = [".", ".."]

--     list =
--       do files <- liftIO (Directory.getDirectoryContents dir)
--          ok $ template "list of files" $
--            toHtml $
--              map (Html.p . toHtml) $
--                filter (`notElem` blacklist) files

--     get file =
--       do res <- liftIO (Directory.doesFileExist file)
--          if res
--          then
--            ok $ template "download file" $
--               undefined
--          else  
--            ok $ template "download file" $
--               p $
--                 toHtml $
--                   "File " ++ file ++ " does not exist"
