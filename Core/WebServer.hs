{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, TypeSynonymInstances,
  OverloadedStrings, TupleSections #-}
module Core.WebServer where

import Control.Monad.IO.Class (liftIO)
import Data.Typeable (Typeable)

import Happstack.Lite (ServerPart, Response, Browsing(..))
import qualified Happstack.Lite as Happstack
import qualified Happstack.Server as Happstack (uriRest)

import qualified Core as Core
import Data.Module
import Monad.InterpreterM
import qualified Stage.Interpreter as Interpreter

coreWebServerName :: String
coreWebServerName = "Core.WebServer"

coreWebServerError :: String -> a
coreWebServerError = error . ((coreWebServerName ++ ".") ++)

data Reply
  = DirectoryReply String
  | HtmlReply String
    deriving (Typeable)

replyType :: Int
IntVal replyType = Core.link . boxString $ coreWebServerName ++ ".Reply"

isReply :: Val -> Bool
isReply (TypeVal (SeqVal [IntVal typeId, _])) = typeId == replyType
isReply _ = False

mkReply :: Reply -> Val
mkReply t = TypeVal (SeqVal [IntVal replyType, dynVal t])

unReply :: Val -> Reply
unReply val@(TypeVal (SeqVal [_, reply]))
  | isReply  val =
    let Just reply' = unDynVal reply :: Maybe Reply in
    reply'
unReply _ =
  coreWebServerError "unReply: expected Reply"

class MonadVal a where
  runMonadVal :: Val -> a

instance MonadVal (ServerPart Response) where
  runMonadVal val
    | isReply val =
      case unReply val of
        DirectoryReply x ->
          Happstack.serveDirectory DisableBrowsing ["index.html"] x
        HtmlReply x -> do
          Happstack.setHeaderM "Content-type" "text/html"
          Happstack.ok $ Happstack.toResponse x
    | otherwise =
      coreWebServerError $ "MonadVal.runMonadVal: expected Reply as first argument"

replyDirectory :: Val -> Val
replyDirectory val
  | isStringVal val =
    mkReply . DirectoryReply . unboxString $ val
  | otherwise =
    coreWebServerError $ "replyDirectory: expected string as first argument"

replyHtml :: Val -> Val
replyHtml val
  | isStringVal val =
    mkReply . HtmlReply . unboxString $ val
  | otherwise =
    coreWebServerError $ "replyHtml: expected string as first argument"

serve :: Val -> InterpreterM Val
serve (FnVal fn) =
  return . IOVal $ do
    Happstack.serve Nothing $
      Happstack.uriRest $ \uri ->
        runMonadVal =<< (liftIO . Interpreter.liftInterpreterM . fn $ boxString uri)
    return $ SeqVal []
serve _ =
  coreWebServerError "serveApp: expected (String -> Reply) as first argument"

fnDesc :: FnDesc
fnDesc =
  [("serve" , FnVal serve),
   ("replyDirectory" , primitive replyDirectory),
   ("replyHtml" , primitive replyHtml)]

coreWebServerModule :: IO Module
coreWebServerModule = mkCoreModule coreWebServerName ["Core"] fnDesc
