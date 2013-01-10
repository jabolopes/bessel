module Core.Shell where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO hiding (hGetContents)
import System.IO.Strict (hGetContents)
import System.IO.Unsafe
import System.Process hiding (shell)

import Core
import Data.SrcFile
import Data.Type
import Monad.InterpreterM


shell :: Expr -> Expr
shell expr =
    FnExpr $ \(SeqExpr args) -> return $ unsafePerformIO $ do
                        let line = unwords $ stringLiteral expr:map stringLiteral args
                        (hIn, hOut, hErr, p) <- runInteractiveCommand line
                        str <- hGetContents hOut
                        waitForProcess p
                        return $ stringExpr str


srcfile :: SrcFile
srcfile = SrcFile "Core.Shell" ["Core"] Nothing $ Right $
          Map.fromList [("shell", (ArrowT (SeqT CharT) (SeqT CharT), m shell))]