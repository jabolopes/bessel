module Utils where


cross :: [[a]] -> [[a]]
cross [] = []
cross [lst] = [[x] | x <- lst]
cross (lst:lsts) = [x:l | x <- lst, l <- cross lsts]


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