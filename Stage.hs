{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}
module Stage where

import Prelude hiding (mod)

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.List as List

import Data.Definition (Definition(..))
import qualified Data.Definition as Definition
import Data.Expr (Expr(..))
import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem
import Data.Module (ModuleT(..), Module(..))
import qualified Data.Module as Module
import Data.Name (Name)
import qualified Data.Name as Name
import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString
import Data.Source (Source(..))
import qualified Expander.Pattern as Pattern
import Monad.NameT (MonadName)
import qualified Stage.Interpreter as Interpreter (interpret)
import qualified Stage.Expander as Expander
import qualified Stage.Renamer as Renamer (rename)
import qualified Pretty.Data.Expr as Pretty
import qualified Pretty.Data.Source as Pretty
import qualified Pretty.Stage as Pretty
import qualified Utils

-- * Definitions

-- edit: improve by a lot...
definitionName :: Source -> Name
definitionName (FnDefS (IdS name) _ _ _) = name
definitionName (FnDefS (PatS name _) _ _ _) = name
definitionName (FnDefS pat _ _ _) =
  let defns = Pattern.genPatternGetters undefined pat in
  case defns of
    [] -> error $ "Stage.definitionName: " ++
                  PrettyString.toString (Pretty.docSource pat) ++ " " ++
                  PrettyString.toString (Pretty.docSourceList defns)
    _ -> Name.untyped . List.intercalate "+" $ map (\(FnDefS (PatS name _) _ _ _) -> Name.nameStr name) defns
definitionName (TypeDeclS x _) = x
definitionName src =
  error $ "Stage.definitionName: expecting function or type definition" ++
          "\n\n\t src = " ++ PrettyString.toString (Pretty.docSource src) ++ "\n\n"

makeDefinition :: Module -> Source -> Definition
makeDefinition mod source =
  let
    name = Module.modName mod `Name.joinNames` definitionName source
    use = (Module.modName mod, Name.empty)
    uses
      | use `elem` Module.modUses mod = Module.modUses mod
      | otherwise = use:Module.modUses mod
  in
    (Definition.initial name) { defSrc = Right source
                              , defUses = uses
                              }

expandDefinition :: (MonadError PrettyString m, MonadName m) => Module -> Definition -> m [Definition]
expandDefinition _ Definition { defSrc = Left err } =
  throwError $ Pretty.definitionContainsNoSource err
expandDefinition mod def@Definition { defSrc = Right src } =
  mapM cloneDefinition =<< Expander.expand src
  where
    cloneDefinition expr@(FnDecl _ name _) =
      return def { defName = Module.modName mod `Name.joinNames` name
                 , defExp = Right expr
                 }
    cloneDefinition expr =
      throwError . Pretty.definitionIsNotFunction $ Pretty.docExpr expr

-- * Modules

reorderModule :: (MonadError PrettyString m) => FileSystem -> Module -> m (FileSystem, Module)
reorderModule fs mod =
  let
    defs = map (makeDefinition mod) (Module.modDecls mod)
    mod' = Module.ensureDefinitions mod defs
  in
    case Utils.duplicates (map (Name.nameStr . Definition.defName) defs) of
      Nothing -> return (FileSystem.add fs mod', mod')
      Just name -> throwError $ Pretty.duplicateDefinitions (Module.modName mod) name

expandModule :: (MonadError PrettyString m, MonadName m) => FileSystem -> Module -> m (FileSystem, Module)
expandModule fs mod@Module { modType = CoreT } =
  return (fs, mod)
expandModule fs mod =
  do mod' <- expand mod (Module.defsAsc mod)
     return (FileSystem.add fs mod', mod')
  where
    expand :: (MonadError PrettyString m, MonadName m) => Module -> [Definition] -> m Module
    expand mdl [] =
      return mdl
    expand mdl (def:defs) =
      do defs' <- expandDefinition mdl def
         let ord =
               case span (/= Name.nameStr (Definition.defName def)) (Module.modDefOrd mdl) of
                 (xs, y:ys) -> xs ++ [y] ++ map (Name.nameStr . Definition.defName) defs' ++ ys
             mdl' = Module.setDefinitionOrder mdl ord
             mdl'' = Module.ensureDefinitions mdl' defs'
         expand mdl'' defs

renameModule :: (MonadError PrettyString m) => FileSystem -> Module -> m (FileSystem, Module)
renameModule fs mod =
  case Renamer.rename fs mod of
    Left err -> throwError err
    Right mod' -> return (FileSystem.add fs mod', mod')

interpretModule :: MonadIO m => FileSystem -> Module -> m (FileSystem, Module)
interpretModule fs mod =
  do mod' <- liftIO $ Interpreter.interpret fs mod
     return (FileSystem.add fs mod', mod')

stageModule :: (MonadError PrettyString m, MonadIO m, MonadName m) => FileSystem -> Module -> m (FileSystem, Module)
stageModule fs mod =
  do (renFs, renMod) <- stage
     interpretModule renFs renMod
  where
    stage =
      do (reordFs, reordMod) <- reorderModule fs mod
         (expFs, expMod) <- expandModule reordFs reordMod
         renameModule expFs expMod
