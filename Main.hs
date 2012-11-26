module Main where

import Repl


main :: IO ()
main = importPrelude >>= repl