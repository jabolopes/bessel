{-# LANGUAGE NamedFieldPuns #-}
module Data.Context where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Type


data Context =
  Context { syms :: [(String, (Type, Maybe Type))]
          , count :: Int }
  deriving (Show)


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


typeContext :: Context -> String -> Either String Type
typeContext syms name =
    case lookupContext syms name of
      Nothing -> Left $ "\n\n\ttypeContext: name " ++ show name ++ " not bound\n"
      Just (t, Nothing) -> Right t
      Just (_, Just t) -> Right t


isEmptyTypeContext :: Context -> String -> Bool
isEmptyTypeContext syms name =
    case lookupContext syms name of
      Nothing -> error $ "\n\n\tisEmptyTypeContext: name " ++ show name ++ " not bound\n"
      Just (_, Nothing) -> True
      Just _ -> False


dropContext :: Context -> String -> Context
dropContext ctx@Context { syms } name =
  ctx { syms = tail $ dropWhile neqName syms }
  where neqName (name', _) = name /= name'


nothingSyms :: Map String Type -> Context
nothingSyms syms = Context { syms = syms', count = 0 }
  where syms' = map (\(name, t) -> (name, (t, Nothing))) $ Map.toList syms


simpleSyms :: Context -> Map String Type
simpleSyms Context { syms } = Map.fromList $ map simple syms
  where simple (name, (t, Nothing)) = (name, t)
        simple (name, (t, Just t')) = (name, t')