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
import qualified Data.Source as Source
import qualified Expander.Pattern as Pattern
import qualified Stage.Interpreter as Interpreter (interpret, interpretDefinition)
import qualified Parser (parseRepl)
import qualified Stage.Expander as Expander
import qualified Stage.Renamer as Renamer (rename, renameDefinition)
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

splitDefinition :: Module -> Source -> Definition
splitDefinition mod source =
  let
    defName = Name.joinNames (Module.modName mod) (definitionName source)
    use = (Module.modName mod, Name.empty)
    uses
      | use `elem` Module.modUses mod = Module.modUses mod
      | otherwise = use:Module.modUses mod
  in
    (Definition.initial defName) { defSrc = Right source
                                 , defUses = uses
                                 }

expandDefinition :: Module -> Definition -> Either PrettyString [Definition]
expandDefinition mod def =
  expandSrc . Definition.defSrc $ def
  where
    mkDef expr@(FnDecl _ defName _) =
      def { defName = Module.modName mod `Name.joinNames` defName
          , defExp = Right expr
          }
    mkDef _ =
      error "expandDefinition: expand can only return top-level definitions"

    expandSrc (Left err) =
      Left $ Pretty.definitionContainsNoSource err
    expandSrc (Right src) =
      map mkDef <$> Expander.expand src

mkSnippet :: FileSystem -> Source -> Definition
mkSnippet fs source@FnDefS {} =
  let
    defName = Name.joinNames Module.interactiveName $ definitionName source
  in
   case FileSystem.lookup fs Module.interactiveName of
     Nothing -> error $ "Stage.mkSnippet: module " ++ show Module.interactiveName ++ " not found"
     Just mod -> (Definition.initial defName) { defUses = Module.modUses mod
                                              , defSrc = Right source
                                              }
mkSnippet fs source =
  mkSnippet fs $ FnDefS (Source.idS "val") Nothing source []

renameSnippet :: FileSystem -> Definition -> Either PrettyString Definition
renameSnippet fs def =
  case Renamer.renameDefinition fs def of
    Left err -> Left err
    Right x -> Right x

stageDefinition :: FileSystem -> String -> IO (Either PrettyString (FileSystem, [Definition]))
stageDefinition fs ln =
  case stage of
    Left err -> return $ Left err
    Right (renDefs, interactive) -> do
      evalDefs <- mapM (Interpreter.interpretDefinition fs) renDefs
      let interactive' = Module.ensureDefinitions interactive evalDefs
      return $ Right (FileSystem.add fs interactive', evalDefs)
  where
    stage =
      do macro <- case Parser.parseRepl Module.interactiveName ln of
                    Left err -> Left (PrettyString.text err)
                    Right x -> Right x
         let interactive = case FileSystem.lookup fs Module.interactiveName of
                             Nothing -> error $ "Stage.stageDefinition: module " ++ show Module.interactiveName ++ " not found"
                             Just mod -> mod
             def = mkSnippet fs macro
         expDefs <- expandDefinition interactive def
         renDefs <- mapM (renameSnippet fs) expDefs
         Right (renDefs, interactive)

-- * Modules

reorderModule :: FileSystem -> Module -> Either PrettyString (FileSystem, Module)
reorderModule fs mod =
  let
    defs = map (splitDefinition mod) (Module.modDecls mod)
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
    expand mod [] = Right mod
    expand mod (def:defs) =
      do defs' <- expandDefinition mod def
         let ord =
               case span (/= Name.nameStr (Definition.defName def)) (Module.modDefOrd mod) of
                 (xs, y:ys) -> xs ++ [y] ++ map (Name.nameStr . Definition.defName) defs' ++ ys
             mod' = Module.setDefinitionOrder mod ord
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
    Left err -> return $ Left err
    Right (renFs, renMod) -> do
      Right <$> interpretModule renFs renMod
  where
    stage =
      do (reordFs, reordMod) <- reorderModule fs mod
         (expFs, expMod) <- expandModule reordFs reordMod
         renameModule expFs expMod
