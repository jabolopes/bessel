module Stage where

import Prelude hiding (mod)

import Data.Definition (Definition(defName, defMac, defExp, defUnprefixedUses))
import qualified Data.Definition as Definition
import qualified Data.Exception as Exception
import Data.Expr (Expr(..))
import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem
import Data.Module (ModuleT(..), Module(..))
import Data.Macro (Macro(..))
import qualified Data.Module as Module
import qualified Data.TypeName as TypeName (fromTypeName)
import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString (text, toString)
import qualified Interpreter (interpret, interpretDefinition)
import qualified Parser (parseRepl)
import qualified Stage.Expander as Expander (expand)
import qualified Pretty.Stage as Pretty
import qualified Renamer (rename, renameDefinition)

-- Definitions

qualifiedName :: Module -> String -> String
qualifiedName mod name = Module.modName mod ++ "." ++ name

defnName :: Macro -> String
defnName (FnDeclM x _) = x
defnName (TypeDeclM x _) = TypeName.fromTypeName x

splitDefinition :: Module -> Macro -> Definition
splitDefinition mod macro =
  let
    name = qualifiedName mod (defnName macro)
    unprefixed = Module.modName mod:Module.modUnprefixedUses mod
    prefixed = Module.modPrefixedUses mod
  in
    (Definition.initial name) { Definition.defUnprefixedUses = unprefixed
                              , Definition.defPrefixedUses = prefixed
                              , defMac = Right macro }

expandDefinition :: Module -> Definition -> Either PrettyString [Definition]
expandDefinition mod def = expandSrc . Definition.defMac $ def
  where mkDef expr@(FnDecl _ fnName _) =
          def { defName = qualifiedName mod fnName
              , defExp = Right expr }
        mkDef _ =
          error "expandDefinition: expand can only return top-level definitions"

        expandSrc (Left err) =
          Left (Pretty.definitionContainsNoMacro err)
        expandSrc (Right src) =
          do exprs <- Expander.expand src
             Right $ map mkDef exprs

mkSnippet :: FileSystem -> Macro -> Definition
mkSnippet fs macro@(FnDeclM name _) =
  let
    name' = Module.interactiveName ++ "." ++ name
    unprefixed = map Module.modName (FileSystem.toAscList fs)
  in
    (Definition.initial name') { defUnprefixedUses = unprefixed
                               , defMac = Right macro }
mkSnippet fs macro = mkSnippet fs $ FnDeclM "val" macro

renameSnippet :: FileSystem -> Definition -> Either PrettyString Definition
renameSnippet fs def =
  case Renamer.renameDefinition fs def of
    Left err -> Left (PrettyString.text err)
    Right x -> Right x

stageDefinition :: FileSystem -> String -> Either PrettyString (FileSystem, [Definition])
stageDefinition fs ln =
  do macro <- case Parser.parseRepl Module.interactiveName ln of
                Left err -> Left (PrettyString.text err)
                Right x -> Right x
     let interactive = FileSystem.get fs Module.interactiveName
         def = mkSnippet fs macro
     expDefs <- expandDefinition interactive def
     renDefs <- mapM (renameSnippet fs) expDefs
     let evalDefs = map (Interpreter.interpretDefinition fs) renDefs
         interactive' = Module.updateDefinitions interactive evalDefs
     return (FileSystem.add fs interactive', evalDefs)

-- * Modules

reorderModule :: FileSystem -> Module -> (FileSystem, Module)
reorderModule fs mod =
  let mod' = Module.addDefinitions mod $ map (splitDefinition mod) (Module.modDecls mod) in
  (FileSystem.add fs mod', mod')

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
                 mod'' = Module.updateDefinitions mod' defs'
             expand mod'' defs

renameModule :: FileSystem -> Module -> Either String (FileSystem, Module)
renameModule fs mod =
  do mod' <- Renamer.rename fs mod
     return (FileSystem.add fs mod', mod')

interpretModule :: FileSystem -> Module -> (FileSystem, Module)
interpretModule fs mod =
  let mod' = Interpreter.interpret fs mod in
  (FileSystem.add fs mod', mod')

stageModule :: FileSystem -> Module -> IO (Either PrettyString (FileSystem, Module))
stageModule fs mod =
  do let (reordFs, reordMod) = reorderModule fs mod
     putStr "reordered, "
     let (expFs, expMod) = case expandModule reordFs reordMod of
                             Left err -> Exception.throwExpanderException (PrettyString.toString err)
                             Right x -> x
     putStr "expanded, "
     let (renFs, renMod) = case renameModule expFs expMod of
                             Left err -> Exception.throwRenamerException err
                             Right x -> x
     putStr "renamed, "
     let (evalFs, evalMod) = interpretModule renFs renMod
     putStrLn "evaluated"
     return (return (evalFs, evalMod))
