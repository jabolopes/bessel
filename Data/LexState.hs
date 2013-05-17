module Data.LexState where

import Data.Word


data LexState =
     LexState { filename :: String
     	      , beginLine :: Int
     	      , endLine :: Int
	      , input :: (Char, [Word8], String) }


lexState :: String -> String -> LexState
lexState filename str =
  LexState { filename = filename
  	   , beginLine = 1 
	   , endLine = 1
	   , input = ('\n',[],str) }