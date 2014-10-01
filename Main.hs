module Main where

import Data.Foldable (forM_)
import Data.Functor ((<$>))

import Config
import Core (coreModule)
-- import qualified Core.Happstack as Core (happstackModule)
import qualified Core.WebServer as Core (coreWebServerModule)
import qualified Core.Html as Core (coreHtmlModule)
import Data.Exception
import qualified Data.FileSystem as FileSystem (initial)
import Data.Module
import Repl hiding (initialFs)

coreModules :: IO [Module]
-- coreModules = sequence [Core.coreModule, Core.happstackModule]
coreModules = sequence [Core.coreModule, Core.coreHtmlModule, Core.coreWebServerModule]

mainException :: UserException -> IO (Maybe a)
mainException e =
  do putUserException e
     return Nothing

main :: IO ()
main =
  do -- args <- getArgs
    coreModules' <- coreModules
    mstate <- (Just <$> importFile (FileSystem.initial coreModules') preludeName)
              `catchUserException` mainException
    forM_ mstate repl
