module Utils where

import Data.List (intercalate)
import qualified Data.Set as Set

duplicates :: Ord a => [a] -> Maybe a
duplicates = loop Set.empty
  where
    loop _ [] = Nothing
    loop s (x:xs) =
      if Set.member x s then
        Just x
      else
        loop (Set.insert x s) xs

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
