module Data.LexState where

import Data.Token (Srcloc(..))
import Data.Word

data LexState =
     LexState { lexFilename :: String
              , lexBeginLine :: Int
              , lexEndLine :: Int
              , lexInput :: (Srcloc, Char, [Word8], String) }

lexState :: String -> String -> LexState
lexState filename str =
  LexState { lexFilename = filename
           , lexBeginLine = 1
           , lexEndLine = 1
           , lexInput = (Srcloc 0 1 1, '\n', [], str) }
