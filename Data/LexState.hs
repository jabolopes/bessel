module Data.LexState where

import Data.Token (Srcloc(..))
import Data.Word

data LexState =
     LexState { lexFilename :: String
              , lexInput :: (Srcloc, Char, [Word8], String) }

-- Returns a 'LexState' for the given @filename@, @line@, and input 'String'.
-- The @line@ is given because the lexer might be called for input chunks, not
-- necessarily for the whole input.
lexState :: String -> Int -> String -> LexState
lexState filename line str =
  LexState { lexFilename = filename
           , lexInput = (Srcloc line 1, '\n', [], str) }
