{-# LANGUAGE BangPatterns #-}
module Repl where

import Prelude hiding (lex)

import Control.Monad.State
import Data.Char (isSpace)
import Data.Functor ((<$>))
import Data.List (intercalate)
import qualified Data.Map as Map (elems, null)
import Data.Maybe (isNothing)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo)
import System.Console.Readline
import System.IO.Error

import Data.Definition (Definition(..))
import qualified Data.Definition as Definition
import Data.Exception
import Data.Expr (DefnKw(..), Expr(..))
import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem
import Data.Module (Module)
import qualified Data.Module as Module hiding (unprefixedUses)
import qualified Doc.Doc as Doc
import qualified Doc.Definition as Doc
import qualified Doc.Expr as Doc
import qualified Doc.Module as Doc
import Expander (expand, expandDefinition)
import Interpreter (interpret, interpretDefinition)
import Lexer (lexTokens)
import Loader (preload, readFileM)
import Monad.InterpreterM (Val)
import Parser (parseRepl)
import Renamer (rename, renameDefinition)
import Reorderer (reorder)
import Utils (split)


data ReplState =
    ReplState { initialFs :: FileSystem, fs :: FileSystem }


type ReplM a = StateT ReplState IO a


doPutVal :: Bool
doPutVal = True


doPutValT :: Bool
doPutValT = True


putVal :: Either String Val -> IO ()
putVal (Left err) = putStrLn err
putVal (Right val) = when doPutVal (print val)


parserEither :: Either String a -> a
parserEither = either throwParserException id


expanderEither :: Either String a -> a
expanderEither = either throwExpanderException id


renamerEither :: Either String a -> a
renamerEither = either throwRenamerException id


-- edit: the Monad forces the argument to be reduced to normal form
typecheckerEither :: Monad m => Either String a -> m a
typecheckerEither x = return $ either throwTypecheckerException id x


stageFiles :: [Module] -> IO FileSystem
stageFiles mods =
    do putStrLn $ "Staging " ++ show n ++ " modules"
       loop FileSystem.empty mods [1..]
    where n = length mods

          putHeader i =
              putStr $ "[" ++ show i ++ "/" ++ show n ++ "] "

          updateFs fs mod =
              (FileSystem.add fs mod, mod)

          reorderFile fs = updateFs fs . reorder
          expandFile fs = updateFs fs . expanderEither . expand fs
          renameFile fs = updateFs fs . renamerEither . rename fs

          interpretFile fs mod =
              let mod' = interpret fs mod in
              (FileSystem.add fs mod', mod')

          loop fs [] _ = return fs
          loop fs (srcs:srcss) (i:is) =
              do putHeader i
                 putStr $ Module.modName srcs ++ ": "

                 let (reordfs, reords) = reorderFile fs srcs
                 putStr "reordered, "

                 let (expfs, exps) = expandFile reordfs reords
                 putStr "expanded, "

                 let (renfs, rens) = renameFile expfs exps
                 putStr "renamed, "

                 let (evalfs, evals) = interpretFile renfs rens
                 putStrLn "evaluated"

                 loop evalfs srcss is


importFile :: FileSystem -> String -> IO ReplState
importFile fs filename =
    do mods <- preload fs filename
       fs' <- stageFiles mods
       let interactive = Module.mkInteractiveModule mods []
           fs'' = FileSystem.add fs' interactive
       return ReplState { initialFs = fs, fs = fs'' }


mkSnippet :: FileSystem -> Expr -> Definition
mkSnippet fs expr@(FnDecl _ name _) =
  let
    name' = Module.interactiveName ++ "." ++ name
    unprefixed = map Module.modName (FileSystem.toAscList fs)
  in
    (Definition.initial name') { defUnprefixedUses = unprefixed
                               , defSrc = Just expr }

mkSnippet fs expr =
  mkSnippet fs $ FnDecl NrDef "val" expr


renameSnippet :: FileSystem -> Definition -> Either String Definition
renameSnippet fs = renameDefinition fs Module.interactiveName


runSnippetM :: String -> ReplM ()
runSnippetM ln =
  do fs <- fs <$> get
     let expr = parserEither (parseRepl Module.interactiveName ln)
     let def = mkSnippet fs expr
         expDef = expanderEither (expandDefinition fs def)
         renDef = renamerEither (renameSnippet fs expDef)
         evalDef = interpretDefinition fs renDef
         interactive = FileSystem.get fs Module.interactiveName
         interactive' = Module.updateDefinitions interactive [evalDef]
     modify $ \s -> s { fs = FileSystem.add fs interactive' }
     liftIO $ putVal (Definition.defVal evalDef)


showMeM :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> String -> StateT ReplState IO ()
showMeM showAll showBrief showOrd showFree showSrc showExp showRen filename =
    let
        filesM
            | showAll = FileSystem.toAscList . fs <$> get
            | otherwise =
                do fs <- fs <$> get
                   case FileSystem.lookup fs filename of
                     Nothing -> do liftIO $ do
                                     putStrLn $ "module " ++ show filename ++ " has not been staged"
                                     putStrLn $ "staged modules are " ++ intercalate ", " (map Module.modName (FileSystem.toAscList fs))
                                   return []
                     Just mod -> return [mod]
    in
      do mods <- filesM
         let modDoc = Doc.docModules showBrief showOrd showFree showSrc showExp showRen mods
         liftIO $ putStr $ Doc.renderDoc modDoc

showTokensM :: String -> ReplM ()
showTokensM filename =
    liftIO (print . lexTokens filename =<< readFileM filename)

data Flag
  = ShowAll
  | ShowBrief
  | ShowOrd

  | ShowFree
  | ShowSrc
  | ShowExp
  | ShowRen
    deriving (Eq, Show)

options :: [OptDescr Flag]
options = [Option "a" [] (NoArg ShowAll) "Show all",
           Option "b" [] (NoArg ShowBrief) "Show brief",
           Option "o" [] (NoArg ShowOrd) "Show in order",
           Option "" ["free"] (NoArg ShowFree) "Show free names of definition",
           Option "" ["src"] (NoArg ShowSrc) "Show source of definition",
           Option "" ["exp"] (NoArg ShowExp) "Show expanded definition",
           Option "" ["ren"] (NoArg ShowRen) "Show renamed definition"]

runCommandM :: String -> [Flag] -> [String] -> ReplM ()
runCommandM "def" opts nonOpts
  | null nonOpts || '.' `notElem` last nonOpts =
    let
      me | null nonOpts = ""
         | otherwise = last nonOpts
    in
      if not showAll && null me then
        usageM
      else
        showMeM showAll showBrief showOrd showFree showSrc showExp showRen me
  | otherwise =
    do let name = last nonOpts
       fs <- fs <$> get
       case FileSystem.lookupDefinition fs name of
         Nothing -> liftIO $ putStrLn $ "definition " ++ show name ++ " does not exist"
         Just def -> liftIO $ putStrLn $ Doc.renderDoc $ Doc.docDefn showFree showSrc showExp showRen def
  where showAll = ShowAll `elem` opts
        showBrief = ShowBrief `elem` opts
        showOrd = ShowOrd `elem` opts
        showFree = ShowFree `elem` opts
        showSrc = ShowSrc `elem` opts
        showExp = ShowExp `elem` opts
        showRen = ShowRen `elem` opts

        usageM =
          liftIO $ putStrLn $ usageInfo "def [-b] [--free] [--src] [--exp] [--ren] [-o] (-a | <me>)" options

runCommandM "load" _ nonOpts
  | null nonOpts =
    liftIO $ putStrLn ":load [ <me> | <file/me> ]"
  | otherwise =
    do fs <- initialFs <$> get
       liftIO (importFile fs (last nonOpts)) >>= put

runCommandM "l" opts nonOpts =
    runCommandM "load" opts nonOpts

runCommandM _ _ _ =
    liftIO $ putStrLn ":def | :load"


dispatchCommandM :: String -> ReplM ()
dispatchCommandM ln =
    case getOpt Permute options (split ' ' ln) of
      (opts, [], []) -> runCommandM "" opts []
      (opts, nonOpts, []) -> runCommandM (head nonOpts) opts (tail nonOpts)
      (_, _, errs) -> liftIO $ putStr $ intercalate "" errs


promptM :: String -> ReplM ()
promptM ln =
    do liftIO (addHistory ln)
       process ln
    where process (':':x) = dispatchCommandM x
          process x = runSnippetM x


replM :: ReplM Bool
replM =
    do mprompt <- liftIO $ readline "bsl$ "
       case mprompt of 
         Nothing -> return True
         Just ln | ln == ":quit" -> return True
                 | all isSpace ln -> replM
                 | otherwise -> promptM ln >> return False


putUserException :: UserException -> IO ()
putUserException (LoaderException str) =
    putStrLn $ "loader error: " ++ str

putUserException (ExpanderException str) =
    putStrLn $ "renamer error: " ++ str

putUserException (RenamerException str) =
    putStrLn $ "renamer error: " ++ str

putUserException (TypecheckerException str) =
    putStrLn $ "typechecker error: " ++ str

putUserException (InterpreterException str) =
    putStrLn $ "interpreter error: " ++ str

putUserException (LexerException str) =
    putStrLn $ "lexical error: " ++ str

putUserException (ParserException str) =
    putStrLn $ "parse error: " ++ str

putUserException (SignalException str) =
    putStrLn $ "uncaught exception: " ++ str


finallyIOException :: Show a => ReplState -> a -> IO (Bool, ReplState)
finallyIOException state e =
    print e >> return (False, state)


finallyException :: a -> UserException -> IO (Bool, a)
finallyException state e =
    putUserException e >> return (False, state)


repl :: ReplState -> IO ()
repl state =
    do (b, state') <- (runStateT replM state
                       `catchIOError` finallyIOException state)
                      `catchUserException` finallyException state
       unless b (repl state')


batch :: String -> ReplState -> IO ()
batch ln state =
    do _ <- (runStateT (promptM ln >> return True) state
             `catchIOError` finallyIOException state)
            `catchUserException` finallyException state
       return ()
