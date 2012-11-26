module Utils where

import Data.List
import qualified Data.Sequence as Sequence


cross :: [[a]] -> [[a]]
cross [] = []
cross [lst] = [[x] | x <- lst]
cross (lst:lsts) = [x:l | x <- lst, l <- cross lsts]


groupPair :: (a -> a -> Bool) -> [(a,b)] -> [(a,[b])]
groupPair _  [] =  []
groupPair eq (x@(x1,x2):xs) =
    (x1, x2:map snd ys):groupPair eq zs
    where (ys,zs) = span (\(x1',_) -> eq x1 x1') xs


primeId :: String -> String
primeId id = id ++ "'"


singleton :: [a] -> Bool
singleton [_] = True
singleton _ = False


split :: Char -> String -> [String]
split c s =
    case dropWhile (== c) s of
      "" -> []
      s' -> w:split c s''
            where (w, s'') = break (== c) s'