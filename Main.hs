module Main where

import Control.Applicative ((<$>))
import Data.Foldable (forM_)

import Config
import Core (coreModule)
import Data.Exception
import qualified Data.FileSystem as FileSystem (initial)
import Data.Module
import Repl hiding (initialFs)

coreModules :: IO [Module]
coreModules = sequence [Core.coreModule]

mainException :: UserException -> IO (Maybe a)
mainException e =
  do putUserException e
     return Nothing

main :: IO ()
main =
  do coreModules' <- coreModules
     mstate <- (Just <$> importFile (FileSystem.initial coreModules') preludeName)
               `catchUserException` mainException
     forM_ mstate repl
