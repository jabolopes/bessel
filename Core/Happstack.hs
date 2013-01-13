module Core.Happstack where

import Control.Concurrent (killThread, forkIO)
import Control.Monad
import qualified Data.Map as Map (fromList)
import System.IO.Unsafe

import Happstack.Server

import Core
import Data.SrcFile
import Data.Type
import Monad.InterpreterM


-- handlers :: FilePath
handlers path =
    msum [ serveDirectory EnableBrowsing [] path ]


serve :: Expr -> InterpreterM Expr
{-# NOINLINE serve #-}
serve expr =
    return $ unsafePerformIO $ do
      let path = unboxString expr
      -- simpleHTTP nullConf $ handlers $ unboxString expr
      threadId <- forkIO $ simpleHTTP nullConf (serveDirectory EnableBrowsing [] path)
      waitForTermination
      killThread threadId
      return $ SeqExpr []


srcfile :: SrcFile
srcfile = SrcFile "Core.Happstack" ["Core"] Nothing $ Right $
          Map.fromList [("serve", (ArrowT (SeqT CharT) DynT, FnExpr serve))]