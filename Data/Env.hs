module Data.Env where

import Data.Map (Map)
import qualified Data.Map as Map


data Env a = Env (Maybe (Env a)) (Map String a)
           deriving (Show)


empty :: Env a
empty = Env Nothing Map.empty


getBinds :: Env a -> [(String, a)]
getBinds (Env _ binds) = Map.toList binds


addBind :: Env a -> String -> a -> Env a
addBind (Env p binds) str val =
    Env p (Map.insertWithKey insert str val binds)
    where insert _ _ _ = error $ "Env.addBind: key " ++ show str ++ " is already bound"


replaceBind :: Env a -> String -> a -> Env a
replaceBind (Env p binds) str val =
    Env p (Map.alter alter str binds)
        where alter (Just _) = error $ "Env.replaceBind: key " ++ show str ++ " is not bound"
              alter Nothing = Just val


findBind :: Env a -> String -> Maybe a
findBind (Env p binds) str =
    case Map.lookup str binds of
      Nothing -> case p of
                   Nothing -> Nothing
                   Just p' -> findBind p' str
      Just val -> Just val


push :: Env a -> Env a
push env = Env (Just env) Map.empty


difference :: Env a -> Env b -> Env a
difference (Env p binds1) (Env _ binds2) =
    Env p $ Map.difference binds1 binds2


union :: Env a -> Env a -> Env a
union (Env p binds1) (Env _ binds2) =
    Env p $ Map.union binds1 binds2


fromList :: [(String, a)] -> Env a
fromList = Env Nothing . Map.fromList