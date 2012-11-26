{-# LANGUAGE ParallelListComp, TupleSections #-}
module Loader (preload, preloadWithPrelude) where

import Control.Monad.State
import Data.Functor ((<$>))

import Data.Graph
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tree

import Data.Stx
import Data.Pat
import qualified Lexer (lex)
import Parser (parseFile)
import Utils


type LoaderM a = StateT [String] IO a


addModulesM :: String -> LoaderM ()
addModulesM name = (name:) <$> get >>= put


loadModuleM :: (String, [Stx a]) -> LoaderM ()
loadModuleM (name, stxs) =
    do when (name /= "") $ addModulesM name
       mapM_ loadM stxs
    

loadM :: Stx a -> LoaderM ()
loadM (CharStx _) = return ()
loadM (IntStx _) = return ()
loadM (DoubleStx _) = return ()
loadM (SeqStx stxs) = mapM_ loadM stxs
loadM (IdStx _) = return ()
loadM (AppStx stx1 stx2) = loadM stx1 >> loadM stx2
loadM (DefnStx _ _ body) = loadM body
loadM (LambdaStx _ body) = loadM body
loadM (ModuleStx _ mod) = loadModuleM mod >> return ()
loadM (TypeStx _ stxs) = mapM_ loadM stxs
loadM (TypeMkStx _ _) = return ()
loadM (TypeUnStx _ _) = return ()
loadM (TypeIsStx _ _) = return ()
loadM (WhereStx _ mod) = loadModuleM mod >> return ()


loadFileM :: String -> LoaderM [Stx String]
loadFileM filename =
    do str <- liftIO $ readFile $ filename ++ ".fl"
       let tks = Lexer.lex str
           stxs = parseFile tks
       mapM_ loadM stxs
       return stxs


preloadModules :: String -> IO (Map String [Stx String], [(String, String)])
preloadModules filename = preloadModules' Map.empty [] [filename]
    where preloadModules' mp deps [] = return (mp, deps)
          preloadModules' mp deps (mod:mods) =
              case Map.lookup mod mp of
                Nothing -> do (stxs, mods') <- runStateT (loadFileM mod) []
                              let mp' = Map.insert mod stxs mp
                                  deps' = map (mod,) mods' ++ deps
                              preloadModules' mp' deps' $ mods' ++ mods
                Just _ -> preloadModules' mp deps mods


buildNodes :: (Enum a, Num a, Ord k) => [k] -> Map k a
buildNodes mods =
    Map.fromList [ (mod, i) | mod <- mods | i <- [1..] ]


buildEdges :: Ord a => Map a b -> [(a, a)] -> [(b, b)]
buildEdges mp = map (mapt (mp Map.!))
    where mapt fn (a, b) = (fn a, fn b)


rebuildModNames :: [a] -> [Int] -> [a]
rebuildModNames mods vs = [ mods !! (i - 1) | i <- vs ]


buildGraph :: Ord a => [a] -> [(a, a)] -> ([a], [a])
buildGraph mods deps =
    let
        nodes = buildNodes mods
        edges = buildEdges nodes deps
        graph = buildG (1, length mods) edges
    in
      (rebuildModNames mods $ topSort graph,
       cycle $ map levels $ scc graph)
    where cycle [] = []
          cycle (vs:vss) | singleton vs = cycle vss
                         | otherwise = rebuildModNames mods (concat vs)


preload :: String -> IO ([String], Map String [Stx String])
preload filename =
    do (mods, deps) <- preloadModules filename
       let (deps', cycle) = buildGraph (Map.keys mods) deps
       case cycle of
         [] -> return (reverse deps', mods)
         mods -> error $ "cycle in " ++ show mods


preloadWithPrelude :: String -> [Stx String] -> String -> IO ([String], Map String [Stx String])
preloadWithPrelude preludeName prelude filename =
  do (deps, mods) <- preload filename
     let deps' = preludeName:deps
         mods' = Map.insert preludeName prelude $ Map.map preludify mods
     return (deps', mods')
     where preludify stxs = ModuleStx "" ("Prelude", []):stxs