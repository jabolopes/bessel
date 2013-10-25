module Stage where

import Prelude hiding (mod)

import Data.Definition (Definition(defMac, defExp, defUnprefixedUses))
import qualified Data.Definition as Definition
import qualified Data.Exception as Exception
import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem
import Data.Module (ModuleT(..), Module(..))
import Data.Macro (Macro(..))
import qualified Data.Module as Module
import qualified Interpreter (interpret, interpretDefinition)
import qualified Parser (parseRepl)
import qualified Stage.Expander as Expander (expand)
import Data.PrettyString (PrettyString)
import qualified Data.PrettyString as PrettyString (text, toString)
import qualified Pretty.Stage as Pretty
import qualified Reorderer (reorder)
import qualified Renamer (rename, renameDefinition)

-- Snippets

expandDefinition :: Definition -> Either PrettyString Definition
expandDefinition def = expandSrc . Definition.defMac $ def
  where expandSrc (Left err) =
          Left (Pretty.definitionContainsNoMacro err)
        expandSrc (Right src) =
          do expr' <- Expander.expand src
             return def { defExp = Right expr' }

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

stageDefinition :: FileSystem -> String -> Either PrettyString (FileSystem, Definition)
stageDefinition fs ln =
  do macro <- case Parser.parseRepl Module.interactiveName ln of
                Left err -> Left (PrettyString.text err)
                Right x -> Right x
     let def = mkSnippet fs macro
     expDef <- expandDefinition def
     renDef <- renameSnippet fs expDef
     let evalDef = Interpreter.interpretDefinition fs renDef
         interactive = FileSystem.get fs Module.interactiveName
         interactive' = Module.updateDefinitions interactive [evalDef]
     return (FileSystem.add fs interactive', evalDef)

-- * Modules

reorderModule :: FileSystem -> Module -> (FileSystem, Module)
reorderModule fs mod =
  let mod' = Reorderer.reorder mod in
  (FileSystem.add fs mod', mod')

expandModule :: FileSystem -> Module -> Either PrettyString (FileSystem, Module)
expandModule fs mod@Module { modType = CoreT } =
  return (fs, mod)
expandModule fs mod =
  do let defs = Module.defsAsc mod
     defs' <- mapM expandDefinition defs
     let mod' = Module.updateDefinitions mod defs'
     return (FileSystem.add fs mod', mod')

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
