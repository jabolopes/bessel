module Stage where

import Prelude hiding (mod)

import Control.Applicative ((<$>))
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
import qualified Stage.Interpreter as Interpreter (interpret)
import qualified Stage.Expander as Expander
import qualified Stage.Renamer as Renamer (rename)
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

expandDefinition :: Module -> Definition -> Either PrettyString [Definition]
expandDefinition mod def =
  expandSrc . Definition.defSrc $ def
  where
    mkDef expr@(FnDecl _ name _) =
      def { defName = Module.modName mod `Name.joinNames` name
          , defExp = Right expr
          }
    mkDef _ =
      error "expandDefinition: expand can only return top-level definitions"

    expandSrc (Left err) =
      Left $ Pretty.definitionContainsNoSource err
    expandSrc (Right src) =
      map mkDef <$> Expander.expand src

-- * Modules

reorderModule :: FileSystem -> Module -> Either PrettyString (FileSystem, Module)
reorderModule fs mod =
  let
    defs = map (makeDefinition mod) (Module.modDecls mod)
    mod' = Module.ensureDefinitions mod defs
  in
    case Utils.duplicates (map (Name.nameStr . Definition.defName) defs) of
      Nothing -> Right (FileSystem.add fs mod', mod')
      Just name -> Left $ Pretty.duplicateDefinitions (Module.modName mod) name

expandModule :: FileSystem -> Module -> Either PrettyString (FileSystem, Module)
expandModule fs mod@Module { modType = CoreT } =
  return (fs, mod)
expandModule fs mod =
  do mod' <- expand mod (Module.defsAsc mod)
     return (FileSystem.add fs mod', mod')
  where
    expand m [] = Right m
    expand m (def:defs) =
      do defs' <- expandDefinition m def
         let ord =
               case span (/= Name.nameStr (Definition.defName def)) (Module.modDefOrd m) of
                 (xs, y:ys) -> xs ++ [y] ++ map (Name.nameStr . Definition.defName) defs' ++ ys
             mod' = Module.setDefinitionOrder m ord
             mod'' = Module.ensureDefinitions mod' defs'
         expand mod'' defs

renameModule :: FileSystem -> Module -> Either PrettyString (FileSystem, Module)
renameModule fs mod =
  do mod' <- Renamer.rename fs mod
     return (FileSystem.add fs mod', mod')

interpretModule :: FileSystem -> Module -> IO (FileSystem, Module)
interpretModule fs mod =
  do mod' <- Interpreter.interpret fs mod
     return (FileSystem.add fs mod', mod')

stageModule :: FileSystem -> Module -> IO (Either PrettyString (FileSystem, Module))
stageModule fs mod =
  case stage of
    Left err ->
      return $ Left err
    Right (renFs, renMod) ->
      Right <$> interpretModule renFs renMod
  where
    stage =
      do (reordFs, reordMod) <- reorderModule fs mod
         (expFs, expMod) <- expandModule reordFs reordMod
         renameModule expFs expMod
