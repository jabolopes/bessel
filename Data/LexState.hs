module Data.LexState where

import Data.Word

data LexState =
     LexState { lexFilename :: String
              , lexBeginLine :: Int
              , lexEndLine :: Int
              , lexInput :: (Char, [Word8], String) }

lexState :: String -> String -> LexState
lexState filename str =
  LexState { lexFilename = filename
           , lexBeginLine = 1
           , lexEndLine = 1
           , lexInput = ('\n', [], str) }
