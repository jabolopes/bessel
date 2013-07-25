module Utils where

import Data.List (intercalate)


flattenId :: [String] -> String
flattenId = intercalate "."


singleton :: [a] -> Bool
singleton [_] = True
singleton _ = False


split :: Char -> String -> [String]
split c s =
    case dropWhile (== c) s of
      "" -> []
      s' -> w:split c s''
            where (w, s'') = break (== c) s'


splitId :: String -> [String]
splitId = split '.'


rebaseName :: [a] -> [a] -> [a] -> [a]
rebaseName prefix1 prefix2 name =
    let name' = drop (length prefix2) name in
    prefix1 ++ name'