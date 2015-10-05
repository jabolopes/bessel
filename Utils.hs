module Utils where

import Prelude hiding (mod)

import qualified Data.ByteString.Char8 as ByteString
import qualified Data.List as List
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
flattenId = List.intercalate "."

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
splitId = map ByteString.unpack . ByteString.split '.' . ByteString.pack

rebaseName :: [a] -> [a] -> [a] -> [a]
rebaseName prefix1 prefix2 name =
  prefix1 ++ drop (length prefix2) name
