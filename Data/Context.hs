{-# LANGUAGE NamedFieldPuns #-}
module Data.Context where

import Data.Functor ((<$>))
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Type


data Context =
  Context { syms :: [(String, (Type, Maybe Type))]
          , count :: Int }
  deriving (Show)


empty :: Context
empty =
    Context { syms = [] , count = 0 }


initial :: [(String, (Type, Maybe Type))] -> Context
initial syms =
    Context { syms = syms
            , count = length syms }


splitContext :: Context -> String -> (Context, Context)
splitContext ctx@Context { syms } name =
    case span neqName syms of
      (_, []) -> error "splitContext: empty list"
      (syms1, _:syms2) -> (ctx { syms = syms1 }, ctx { syms = syms2 })
    where neqName (name', _) = name /= name'


insertContext :: Context -> String -> (Type, Maybe Type) -> Context
insertContext ctx@Context { syms } name t =
    ctx { syms = (name, t):syms }


updateContext :: Context -> String -> (Type, Maybe Type) -> Context
updateContext ctx name t =
    let (Context { syms = syms1 }, Context { syms = syms2 }) = splitContext ctx name in
    ctx { syms = syms1 ++ [(name, t)] ++ syms2 }


lookupContext :: Context -> String -> Maybe (Type, Maybe Type)
lookupContext Context { syms } name = lookup name syms


typeContext :: Context -> String -> Either String (Context, Type)
typeContext ctx@Context { count } name =
    do (count', t') <- freshForallT count <$> t
       return (ctx { count = count' }, t')
    where t = case lookupContext ctx name of
                Nothing -> Left $ "\n\n\ttypeContext: name " ++ show name ++ " not bound\n"
                Just (t, Nothing) -> Right t
                Just (_, Just t) -> Right t


isEmptyTypeContext :: Context -> String -> Bool
isEmptyTypeContext ctx name =
    case lookupContext ctx name of
      Nothing -> error $ "\n\n\tisEmptyTypeContext: name " ++ show name ++ " not bound\n"
      Just (_, Nothing) -> True
      Just _ -> False


dropContext :: Context -> String -> Context
dropContext ctx@Context { syms } name =
  ctx { syms = tail $ dropWhile neqName syms }
  where neqName (name', _) = name /= name'


nothingSyms :: Map String Type -> Context
-- nothingSyms syms = Context { syms = syms', count = 0 }
nothingSyms syms = resetCount (Context { syms = syms', count = 97 })
  where syms' = map (\(name, t) -> (name, (t, Nothing))) $ Map.toList syms


resetCount :: Context -> Context
resetCount ctx = ctx { count = 97 }


simpleSyms :: Context -> [(String, Type)]
simpleSyms Context { syms } = map simple syms
  where simple (name, (t, Nothing)) = (name, t)
        simple (name, (_, Just t')) = (name, t')


isWellformed :: Context -> Type -> Type -> Bool
isWellformed Context { syms } (EvarT var1) (EvarT var2) =
  let
    i1 = var1 `findT` syms
    i2 = var2 `findT` syms
  in
   i1 <= i2
  where findT name = findIndex (\(x, _) -> x == name)

isWellformed _ _ _ = True
