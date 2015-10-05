{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiWayIf,
             TypeSynonymInstances, OverloadedStrings, TupleSections #-}
module Core.WebServer where

import Data.Typeable (Typeable)

import Happstack.Lite (ServerPart, Response, Browsing(..))
import qualified Happstack.Lite as Happstack
import qualified Happstack.Server as Happstack (uriRest)
import Happstack.Server.Monads

import qualified Core as Core
import Data.Module
import Data.Name (Name)
import qualified Data.Name as Name
import Monad.InterpreterM
import qualified Stage.Interpreter as Interpreter

coreWebServerName :: Name
coreWebServerName = Name.untyped "Core.WebServer"

coreWebServerError :: String -> a
coreWebServerError = error . ((Name.nameStr coreWebServerName ++ ".") ++)

data Reply = Reply (ServerPart Response)
           deriving (Typeable)

replyType :: Int
IntVal replyType = Core.link . boxString $ Name.nameStr coreWebServerName ++ ".Reply"

isReply :: Val -> Bool
isReply (TypeVal (SeqVal [IntVal typeId, _])) = typeId == replyType
isReply _ = False

mkReply :: ServerPart Response -> Val
mkReply t = TypeVal (SeqVal [IntVal replyType, dynVal (Reply t)])

unReply :: Val -> ServerPart Response
unReply val@(TypeVal (SeqVal [_, reply]))
  | isReply val =
    let Just (Reply reply') = unDynVal reply :: Maybe Reply in
    reply'
unReply _ =
  coreWebServerError "unReply: expected Reply"

class MonadVal a where
  runMonadVal :: Val -> a

instance MonadVal (ServerPart Response) where
  runMonadVal val
    | isReply val =
      unReply val
    | otherwise =
      coreWebServerError $ "MonadVal.runMonadVal: expected Reply as first argument"

replyDirectory :: Val -> Val
replyDirectory val
  | isStringVal val =
    mkReply . Happstack.serveDirectory DisableBrowsing ["index.html"] $ unboxString val
  | otherwise =
    coreWebServerError $ "replyDirectory: expected string as first argument"

replyHtml :: Val -> Val
replyHtml val
  | isStringVal val =
    mkReply $ do
      Happstack.setHeaderM "Content-type" "text/html"
      Happstack.ok . Happstack.toResponse $ unboxString val
  | otherwise =
    coreWebServerError $ "replyHtml: expected string as first argument"

-- lookText arg =
--   mkReply $ do
--     text <- Happstack.lookText arg
--     return . boxString $ Text.unpack text

-- serve :: Val -> InterpreterM Val
-- serve (FnVal fn) =
--   return . IOVal $ do
--     Happstack.serve Nothing $
--       Happstack.uriRest $ \uri -> do
--         liftIO $ putStrLn uri
--         val <- liftIO . Interpreter.liftInterpreterM . fn $ boxString uri
--         if | isReply val -> unReply val
--            | otherwise -> coreWebServerError $ "serve: expected function of type 'String -> Reply' as first argument"
--     return $ SeqVal []
-- serve _ =
--   coreWebServerError "serveApp: expected (String -> Reply) as first argument"

fnDesc :: FnDesc
fnDesc =
  [-- ("serve" , FnVal serve),
   ("replyDirectory" , primitive replyDirectory),
   ("replyHtml" , primitive replyHtml)]

coreWebServerModule :: IO Module
coreWebServerModule = mkCoreModule coreWebServerName [Name.untyped "Core"] fnDesc
