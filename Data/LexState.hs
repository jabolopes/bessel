module Data.LexState where

import Data.Token (Srcloc(..))
import Data.Word

data LexState =
     LexState { lexFilename :: String
              , lexInput :: (Srcloc, Char, [Word8], String) }

lexState :: String -> String -> LexState
lexState filename str =
  LexState { lexFilename = filename
           , lexInput = (Srcloc 1 1, '\n', [], str) }
