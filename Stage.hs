module Stage where

import Prelude hiding (mod)

import Control.Applicative ((<$>))

import Data.Definition (Definition(..))
import qualified Data.Definition as Definition
import Data.Expr (Expr(..))
import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem
import Data.Module (ModuleT(..), Module(..))
import qualified Data.Module as Module
import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString (text)
import qualified Data.QualName as QualName (fromQualName)
import Data.Source (Source(..))
import qualified Stage.Interpreter as Interpreter (interpret, interpretDefinition)
import qualified Parser (parseRepl)
import qualified Stage.Expander as Expander (expand)
import qualified Stage.Renamer as Renamer (rename, renameDefinition)
import qualified Pretty.Stage as Pretty
import qualified Utils

-- Definitions

qualifiedName :: Module -> String -> String
qualifiedName mod name = Module.modName mod ++ "." ++ name

defnName :: Source -> String
defnName (FnDeclS x _) = x
defnName (TypeDeclS x _) = QualName.fromQualName x

splitDefinition :: Module -> Source -> Definition
splitDefinition mod source =
  let
    name = qualifiedName mod (defnName source)
    use = (Module.modName mod, "")
    uses
      | use `elem` Module.modUses mod = Module.modUses mod
      | otherwise = use:Module.modUses mod
  in
    (Definition.initial name) { defSrc = Right source
                              , defUses = uses }

expandDefinition :: Module -> Definition -> Either PrettyString [Definition]
expandDefinition mod def = expandSrc . Definition.defSrc $ def
  where mkDef expr@(FnDecl _ fnName _) =
          def { defName = qualifiedName mod fnName
              , defExp = Right expr }
        mkDef _ =
          error "expandDefinition: expand can only return top-level definitions"

        expandSrc (Left err) =
          Left (Pretty.definitionContainsNoSource err)
        expandSrc (Right src) =
          do exprs <- Expander.expand src
             Right $ map mkDef exprs

mkSnippet :: FileSystem -> Source -> Definition
mkSnippet fs source@(FnDeclS name _) =
  let name' = Module.interactiveName ++ "." ++ name in
  case FileSystem.lookup fs Module.interactiveName of
    Nothing -> error $ "Stage.mkSnippet: module " ++ Module.interactiveName ++ " not found"
    Just mod -> (Definition.initial name') { defUses = Module.modUses mod
                                           , defSrc = Right source }
mkSnippet fs source = mkSnippet fs $ FnDeclS "val" source

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
                             Nothing -> error $ "Stage.stageDefinition: module " ++ Module.interactiveName ++ " not found"
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
    case Utils.duplicates (map Definition.defName defs) of
      Nothing -> Right (FileSystem.add fs mod', mod')
      Just name -> Left $ Pretty.duplicateDefinitions (Module.modName mod) name

expandModule :: FileSystem -> Module -> Either PrettyString (FileSystem, Module)
expandModule fs mod@Module { modType = CoreT } =
  return (fs, mod)
expandModule fs mod =
  do mod' <- expand mod (Module.defsAsc mod)
     return (FileSystem.add fs mod', mod')
  where expand mod [] = Right mod
        expand mod (def:defs) =
          do defs' <- expandDefinition mod def
             let ord =
                   case span (/= Definition.defName def) (Module.modDefOrd mod) of
                     (xs, y:ys) -> xs ++ [y] ++ map Definition.defName defs' ++ ys
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
