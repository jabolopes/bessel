{-# LANGUAGE NamedFieldPuns #-}
module Data.Context where

import Data.Functor ((<$>))
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Type


data Context =
  Context { syms :: [(String, Type)]
          , count :: Int }
  deriving (Show)


empty :: Context
empty =
    Context { syms = [] , count = 0 }


initial :: [(String, Type)] -> Context
initial syms =
    Context { syms = syms
            , count = length syms }


splitContext :: Context -> String -> (Context, Context)
splitContext ctx@Context { syms } name =
    case span neqName syms of
      (_, []) -> error "splitContext: empty list"
      (syms1, _:syms2) -> (ctx { syms = syms1 }, ctx { syms = syms2 })
    where neqName (name', _) = name /= name'


insertContext :: Context -> String -> Type -> Context
insertContext ctx@Context { syms } name t =
    ctx { syms = (name, t):syms }


updateContext :: Context -> String -> Type -> Context
updateContext ctx name t =
    let (Context { syms = syms1 }, Context { syms = syms2 }) = splitContext ctx name in
    ctx { syms = syms1 ++ [(name, t)] ++ syms2 }


lookupContext :: Context -> String -> Maybe Type
lookupContext Context { syms } name = lookup name syms


freshType :: Context -> Type -> (Context, Type)
freshType ctx@Context { count } typ =
  let (count', typ') = freshForallT count typ in
  (ctx { count = count' }, typ')


-- edit: needed a 'freshForallT' in all calls to 'typeContext'?
-- edit: reuse function 'freshType' ?
typeContext :: Context -> String -> Either String (Context, Type)
typeContext ctx@Context { count } name =
    do (count', t') <- freshForallT count <$> t
       return (ctx { count = count' }, t')
    where t = case lookupContext ctx name of
                Nothing -> Left $ "\n\n\ttypeContext: name " ++ show name ++ " not bound\n"
                Just t -> Right t

isEmptyTypeContext :: Context -> String -> Bool
isEmptyTypeContext ctx name =
    case lookupContext ctx name of
      Nothing -> error $ "\n\n\tisEmptyTypeContext: name " ++ show name ++ " not bound\n"
      Just (EvarT name') | name == name' -> True
      Just _ -> False


dropContext :: Context -> String -> Context
dropContext ctx@Context { syms } name =
  ctx { syms = tail $ dropWhile neqName syms }
  where neqName (name', _) = name /= name'


nothingSyms :: Map String Type -> Context
nothingSyms syms =
    Context { syms = Map.toList freshSyms, count = count' }
    where (count', freshSyms) = Map.mapAccum freshForallT 97 syms


resetCount :: Context -> Context
resetCount ctx = ctx { count = 97 }


-- edit: this should do a full substitution of existential variables
simpleSyms :: Context -> [(String, Type)]
simpleSyms Context { syms } = syms


isWellformed' :: Context -> String -> String -> Bool
isWellformed' Context { syms } var1 var2 =
  let
    i1 = var1 `findT` syms
    i2 = var2 `findT` syms
  in
   i1 <= i2
  where findT name = findIndex (\(x, _) -> x == name)


-- @isWellFormed ctx t1 t2@ verifies that @t2@ is already in context
-- (i.e., occurs before @t1@) in order for the assignment to @t1@ to
-- be valid.
isWellformed :: Context -> Type -> Type -> Bool
isWellformed ctx (EvarT var1) (EvarT var2) =
  isWellformed' ctx var1 var2

isWellformed ctx (EvarT var1) (TvarT var2) =
  isWellformed' ctx var1 var2

-- edit: i'm not sure if this is necessary...
-- isWellformed ctx (TvarT var1) (EvarT var2) =
--   isWellformed' ctx var1 var2

-- edit: simple types (BoolT, IntT, ...) should be in the
-- environment...
isWellformed _ _ BoolT   = True
isWellformed _ _ IntT    = True
isWellformed _ _ DoubleT = True
isWellformed _ _ CharT   = True

-- edit: i'm not sure if this is necessary...
-- isWellformed ctx t1 (SeqT t2) =
--   isWellformed ctx t1 t2

isWellformed _ _ DynT    = True

isWellformed _ t1@(EvarT _) t2 =
  error $ "isWellformed: unhandled case" ++
          "\n\n\t t1 = " ++ show t1 ++
          "\n\n\t t2 = " ++ show t2 ++ "\n"

isWellformed _ t1 t2 =
  error $ "isWellformed: t1 must be an evar" ++
          "\n\n\t t1 = " ++ show t1 ++
          "\n\n\t t2 = " ++ show t2 ++ "\n"