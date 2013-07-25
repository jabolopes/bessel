{-# LANGUAGE ParallelListComp #-}
module Expander where

import Control.Monad.Error (throwError)
import Control.Monad.State
import Data.Functor ((<$>))
import Data.List (intercalate, isPrefixOf, maximumBy, nub, partition, sort)

import Data.Definition (Definition(..))
import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem
import Data.SrcFile (SrcFileT(..), SrcFile(..))
import qualified Data.SrcFile as SrcFile
import Data.Stx
import Data.Type


data ExpanderState =
    ExpanderState { macroCount :: Int }

initialExpanderState =
    ExpanderState { macroCount = 0 }


type ExpanderM a = StateT ExpanderState (Either String) a


genNameM :: String -> ExpanderM String
genNameM name =
    do c <- macroCount <$> get
       modify $ \s -> s { macroCount = macroCount s + 1 }
       return (name ++ "#" ++ show c)


-- cond macros

-- |
-- Generates a function definition for a pattern.
--
-- @
-- xs@[x@, y@]
-- @
--
-- @
-- def x = head xs
-- def y = head (tail xs)
-- @
mkPatDefns :: Stx a -> [PatDefn a] -> [Stx a]
mkPatDefns val = map (mkDefn `uncurry`)
    where mkDefn name mods =
              DefnStx Nothing NrDef name (foldAppStx val mods)


-- |
-- @
-- x@pat y@pat = body
-- @
--
-- @
-- body where { def x = ...
--              def y = ... }
-- @
lambdaBody :: [a] -> [Pat a] -> Stx a -> Stx a
lambdaBody args pats body =
    let
        defns = [ (arg, patDefns pat) | arg <- args | pat <- pats ]
        defns' = concat [ mkPatDefns (IdStx arg) patDefns | (arg, patDefns) <- defns ]
    in
      if null defns' then
          body
      else
          WhereStx body defns'


-- |
-- @
-- x@pat1 y@pat1' -> body1
-- x@pat2 y@pat2' -> body2
-- ...
-- _ -> blame "..."
-- @
--
-- @
-- cond
--   pred1 x && pred2 y -> body1 where { x = ... && y = ... }
--   pred1 x && pred2 y -> body1 where { x = ... && y = ... }
--   _ _ -> blame "..."
-- @
--
-- The generated code above is a simplified representation because
-- '&&' is encoded through 'CondStx'.
expandCondMacro :: [([Pat String], Stx String)] -> String -> ExpanderM (Stx String)
expandCondMacro ms blame =
    do let npats = length $ fst $ maximumBy (\x y -> compare (length (fst x)) (length (fst y))) ms
       args <- replicateM npats (genNameM "arg")
       let (patss, exprs) = unzip ms
           preds = map (combinePreds args) patss
       exprs' <- mapM expandOneM exprs
       let exprs'' = zipWith (lambdaBody args) patss exprs'
           ms' = zip preds exprs''
       returnOneM $ lambdas args (CondStx ms' blame)
    where lambdas [] body = body
          lambdas (arg:args) body =
              LambdaStx arg Nothing (lambdas args body)

          applyPred arg pat =
              AppStx (patPred pat) (IdStx arg)

          alignArgs args pats =
              drop (length args - length pats) args

          combinePreds args pats =
              foldl1 andStx (zipWith applyPred (alignArgs args pats) pats)

-- /cond macros


-- cotype macros

cotypeObservation :: Int -> Observation -> Int -> Stx String
cotypeObservation tid (name, obT) i =
    let
        t = ArrowT DynT obT
        isStx = appStx "is#" (IntStx tid)
        argPat = namePat "arg" (mkPredPat isStx)
        condBody = foldAppStx (IdStx "arg") [appStx "index" (IntStx i), IdStx "un#"]
        body = CondMacro [([argPat], condBody)] name
    in
      DefnStx (Just t) NrDef name body


cotypeObservations :: Int -> [Observation] -> [Stx String]
cotypeObservations tid obs = zipWith (cotypeObservation tid) obs [0..]

-- /cotype macros

oneM :: Stx String -> Stx String
oneM = id

-- edit: in case someday we want to return multiple stxs in expand
expandOneM :: Stx String -> ExpanderM (Stx String)
expandOneM = expandM
-- expandOneM stx = head <$> expandM stx

-- edit: in case someday we want to return multiple stxs in expand
returnOneM :: Stx String -> ExpanderM (Stx String)
returnOneM = return . oneM


expandM :: Stx String -> ExpanderM (Stx String)
expandM stx@(CharStx _) = returnOneM stx
expandM stx@(IntStx _) = returnOneM stx
expandM stx@(DoubleStx _) = returnOneM stx
expandM (SeqStx stxs) = oneM . SeqStx <$> mapM expandOneM stxs

expandM stx@(IdStx _) = returnOneM stx

expandM (AppStx stx1 stx2) =
    oneM <$> ((AppStx <$> expandOneM stx1) `ap` expandOneM stx2)

expandM (CondMacro ms blame) =
    expandCondMacro ms blame

expandM (CondStx ms blame) =
    oneM . (`CondStx` blame) <$> mapM expandMatch ms
    where expandMatch (stx1, stx2) =
              do stx1' <- expandOneM stx1
                 stx2' <- expandOneM stx2
                 return (stx1', stx2')

expandM (CotypeStx name obs ns) = undefined
    -- do srcfile <- srcfile <$> get
    --    tid <- genTypeIdM
    --    addCotypeSymbolM name (SrcFile.name srcfile) tid obs
    --    expandM (cotypeObservationsModule name tid obs ns)
    -- where cotypeId =
    --           DefnStx (Just IntT) NrDef "typeId" (IntStx 0)
          
    --       cotypeObservationsModule name tid obs (Namespace uses stxs) =
    --           ModuleStx [name] (Namespace uses (cotypeId:cotypeObservations tid obs ++ stxs))

expandM (DefnStx t kw name body) =
    oneM . DefnStx t kw name <$> expandOneM body

expandM (LambdaMacro typePats body) =
    returnOneM $ lambdas typePats body
    where lambdas [] body = body
          lambdas (pat:pats) body =
              let
                  arg = fst (head (patDefns pat))
                  IdStx ann = patPred pat
              in
                LambdaStx arg (Just ann) (lambdas pats body)

expandM (LambdaStx arg ann body) =
    oneM . LambdaStx arg ann <$> expandOneM body

expandM (MergeStx name vals) =
    oneM . MergeStx name <$> mapM expandVals vals
    where expandVals (key, stx) =
              do stx' <- expandOneM stx
                 return (key, stx')

expandM (ModuleStx prefix ns) =
    undefined

expandM (WhereStx body defns) =
    do body' <- expandOneM body
       oneM . WhereStx body' <$> mapM expandM defns


expandDefinitionM :: Definition -> ExpanderM Definition
expandDefinitionM def@Definition { srcStx = Just stx } =
    do stx' <- expandOneM stx
       return def { expStx = Just stx' }


expandDefinition :: FileSystem -> Definition -> Either String Definition
expandDefinition fs def =
    fst <$> runStateT (expandDefinitionM def) initialExpanderState 


expandDefinitions :: FileSystem -> SrcFile -> [Definition] -> Either String SrcFile
expandDefinitions _ srcfile [] = return srcfile

expandDefinitions fs srcfile (def:defs) =
    do def' <- expandDefinition fs def
       let srcfile' = SrcFile.updateDefinitions srcfile [def']
           fs' = FileSystem.add fs srcfile'
       expandDefinitions fs' srcfile' defs


expand :: FileSystem -> SrcFile -> Either String SrcFile
expand _ srcfile@SrcFile { t = CoreT } =
    return srcfile

expand fs srcfile =
    expandDefinitions fs srcfile (SrcFile.defsAsc srcfile)