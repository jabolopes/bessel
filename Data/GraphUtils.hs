module Data.GraphUtils where

import Data.Graph (Edge, buildG, topSort, scc)
import Data.Tree (levels)

import Utils

hasCycle :: [[[a]]] -> Maybe [a]
hasCycle [] = Nothing
hasCycle (is:iss)
    | singleton is = hasCycle iss
    | otherwise = Just (concat is)

replaceIndexes :: [a] -> [Int] -> [a]
replaceIndexes nodes is = [ nodes !! (i - 1) | i <- is ]

acyclicTopSort :: [a] -> [Edge] -> Either [a] [a]
acyclicTopSort nodes edges =
  let graph = buildG (1, length nodes) edges in
  case hasCycle $ map levels $ scc graph of
    Nothing -> Right $ reverse $ replaceIndexes nodes $ topSort graph
    Just is -> Left (replaceIndexes nodes is)
