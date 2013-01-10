module Main where

import qualified Core (namespace)
import Repl


main :: IO ()
main = importFile [Core.namespace] "Prelude" >>= repl