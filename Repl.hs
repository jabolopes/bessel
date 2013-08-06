{-# LANGUAGE BangPatterns #-}
module Repl where

import Prelude hiding (lex)

import Control.Monad.State
import Data.Char (isSpace)
import Data.Functor ((<$>))
import Data.List (intercalate, isPrefixOf, nub)
import Data.Map (Map)
import qualified Data.Map as Map (elems, null, union)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt)
import System.Console.Readline
import System.IO.Error

import Config
import qualified Data.Env as Env (getBinds)
import Data.Definition (Definition(..))
import qualified Data.Definition as Definition
import Data.Exception
import Data.Expr
import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem
import Data.Maybe
import Data.SrcFile (SrcFile)
import qualified Data.SrcFile as SrcFile hiding (unprefixedUses)
import Data.Symbol
import Data.Type
import Expander (expand, expandDefinition)
import Interpreter
import Lexer
import Linker
import Loader
import Monad.InterpreterM
import Parser
import Printer.PrettyExpr
import Renamer (rename, renameDefinition)
import Reorderer (reorder)
import Typechecker
import Utils


data ReplState =
    ReplState { initialFs :: FileSystem, fs :: FileSystem }


type ReplM a = StateT ReplState IO a


doPutLine = False
doPutTokens = True
doPutParsedExpr = True
doPutVal = True
doPutValT = True
doPutEnvironment = False


putLine :: String -> IO ()
putLine str =
    when doPutLine $
      putStrLn $ "> " ++ str


putParsedExpr :: Expr -> IO ()
putParsedExpr expr =
    when doPutParsedExpr $ do
      putStrLn "> Parsed expr"
      prettyPrint expr
      putStrLn ""
      putStrLn ""


putVal :: Show a => a -> IO ()
putVal val =
  when doPutVal (print val)


putValT :: (Show a, Show b) => a -> b -> IO ()
putValT val t =
  when doPutValT $
    putStrLn $ show val ++ " :: " ++ show t


putEnvironment :: Show a => a -> IO ()
putEnvironment env =
  when doPutEnvironment $ do
    putStrLn ""
    putStrLn "Environment"
    print env
    putStrLn ""


parserEither :: Either String a -> a
parserEither = either throwParserException id


expanderEither :: Either String a -> a
expanderEither = either throwExpanderException id


renamerEither :: Either String a -> a
renamerEither = either throwRenamerException id


-- edit: the Monad forces the argument to be reduced to normal form
typecheckerEither :: Monad m => Either String a -> m a
typecheckerEither x = return $ either throwTypecheckerException id x


stageFiles :: [SrcFile] -> IO FileSystem
stageFiles srcfiles =
    do liftIO $ putStrLn $ "Staging " ++ show n ++ " namespaces"
       link =<< loop FileSystem.empty srcfiles [1..]
    where n = length srcfiles

          putHeader i =
              putStr $ "[" ++ show i ++ "/" ++ show n ++ "] "

          updateFs fs !srcfile =
              (FileSystem.add fs srcfile, srcfile)

          reorderFile fs = updateFs fs . reorder
          expandFile fs = updateFs fs . expanderEither . expand fs
          renameFile fs = updateFs fs . renamerEither . rename fs

          typecheckFile fs srcfile =
              updateFs fs <$> typecheckerEither (typecheck fs srcfile)

          interpretFile fs srcfile =
              let srcfile' = interpret fs srcfile in
              (FileSystem.add fs srcfile', srcfile')

          link fs =
              do let srcfiles' = map (FileSystem.get fs . SrcFile.name) srcfiles
                     fs' = FileSystem.initial (linkSrcFiles srcfiles')
                 putStrLn "linked"
                 return fs'

          loop fs [] _ = return fs
          loop fs (srcs:srcss) (i:is) =
              do putHeader i
                 putStr $ SrcFile.name srcs ++ ": "

                 let (reordfs, reords) = reorderFile fs srcs
                 putStr "reordered, "

                 let (expfs, exps) = expandFile reordfs reords
                 putStr "expanded, "

                 let (renfs, rens) = renameFile expfs exps
                 putStr "renamed, "

                 (typfs, typs) <- typecheckFile renfs rens
                 putStr "typechecked, "

                 let (evalfs, evals) = interpretFile typfs typs
                 putStrLn "evaluated"

                 loop evalfs srcss is


importFile :: FileSystem -> String -> IO ReplState
importFile fs filename =
    do srcfiles <- preload fs filename
       fs' <- stageFiles srcfiles
       let interactive = SrcFile.mkInteractiveSrcFile srcfiles []
           fs'' = FileSystem.add fs' interactive
       return ReplState { initialFs = fs, fs = fs'' }


mkSnippet :: FileSystem -> Expr -> Definition
mkSnippet fs expr@(FnDecl _ name _) =
  let
    name' = SrcFile.interactiveName ++ "." ++ name
    unprefixed = map SrcFile.name (FileSystem.toAscList fs)
  in
    (Definition.initial name') { unprefixedUses = unprefixed
                               , srcExpr = Just expr }

mkSnippet fs expr =
  mkSnippet fs $ FnDecl NrDef "val" expr


renameSnippet :: FileSystem -> Definition -> Either String Definition
renameSnippet fs = renameDefinition fs SrcFile.interactiveName


runSnippetM :: String -> ReplM ()
runSnippetM ln =
  do fs <- fs <$> get
     let expr = parserEither (parseRepl SrcFile.interactiveName ln)
     liftIO (putParsedExpr expr)
     let def = mkSnippet fs expr
         expDef = expanderEither (expandDefinition fs def)
         renDef = renamerEither (renameSnippet fs expDef)
     typDef <- typecheckerEither (typecheckDefinitionM fs renDef)
     -- edit: lazy evaluation problems with interpretDefinition
     let evalDef = interpretDefinition fs typDef
         interactive = FileSystem.get fs SrcFile.interactiveName
         interactive' = SrcFile.updateDefinitions interactive [evalDef]
     modify $ \s -> s { fs = FileSystem.add fs interactive' }
     liftIO $ putValM (Definition.typ evalDef) (Definition.val evalDef)
  where putValM Nothing val = putVal val
        putValM (Just t) (Just val) = putValT val t


showModuleM :: Bool -> Bool -> Bool -> String -> ReplM ()
showModuleM showAll showBrief showOrd filename =
    let
        filesM
            | showAll = FileSystem.toAscList . fs <$> get
            | otherwise =
                do fs <- fs <$> get
                   case FileSystem.lookup fs filename of
                     Nothing -> do liftIO $ do
                                     putStrLn $ "namespace " ++ show filename ++ " has not been staged"
                                     putStrLn $ "staged namespaces are " ++ intercalate ", " (map SrcFile.name (FileSystem.toAscList fs))
                                   return []
                     Just srcfile -> return [srcfile]
    in
      filesM >>= (liftIO . mapM_ (putStrLn . showSrcFile))
    where showDeps srcfile
              | null (SrcFile.deps srcfile) = ""
              | otherwise = " uses " ++ intercalate ", " (SrcFile.deps srcfile)

          showDefns srcfile
              | Map.null (SrcFile.defs srcfile) = "\n no definitions"
              | showOrd = "\n " ++ intercalate "\n " (map show . SrcFile.defsAsc $ srcfile)
              | otherwise = "\n " ++ intercalate "\n " (map show . Map.elems . SrcFile.defs $ srcfile)

          showSrcFile srcfile =
              let str = SrcFile.name srcfile ++ " (" ++ show (SrcFile.t srcfile) ++ ")" ++ showDeps srcfile in
              if showBrief then
                  str
              else
                  str ++ showDefns srcfile


showTokensM :: String -> ReplM ()
showTokensM filename =
    liftIO (print . lexTokens filename =<< readFileM filename)


showRenamedM :: Bool -> String -> ReplM ()
showRenamedM showAll filename =
    let
        filesM
            | showAll = FileSystem.toAscList . fs <$> get
            | otherwise =
                do fs <- fs <$> get
                   case FileSystem.lookup fs filename of
                     Nothing -> do liftIO $ do
                                     putStrLn $ "namespace " ++ show filename ++ " has not been staged"
                                     putStrLn $ "staged namespaces are " ++ intercalate ", " (map SrcFile.name (FileSystem.toAscList fs))
                                   return []
                     Just srcfile -> return [srcfile]
    in
      filesM >>= liftIO . mapM_ prettyPrintSrcFile


showDefinition :: Definition -> String
showDefinition def =
  name def ++ " (" ++
  showSym def ++ ") :: " ++
  showTyp def ++ " = " ++
  showVal def ++ "\n\n" ++
  showFreeNames def
  where indent = (" " ++)
        showSym = show . fromJust . Definition.symbol
        showTyp = show . fromJust . Definition.typ
        showVal = show . fromJust . Definition.val
        showFreeNames = intercalate ", " . Definition.freeNames


putDefinitionM :: Definition -> ReplM ()
putDefinitionM def =
  liftIO $ do
    putStrLn (showDefinition def)
    putStrLn ""
    prettyPrint (fromJust (srcExpr def))
    putStrLn ""
    putStrLn ""
    prettyPrint (fromJust (expExpr def))
    putStrLn ""
    putStrLn ""
    prettyPrint (fromJust (renExpr def))
    putStrLn ""
    putStrLn ""
    prettyPrint (fromJust (typExpr def))
    putStrLn ""


data Flag
    = ShowAll
    | ShowBrief
    | ShowOrd
      deriving (Eq, Show)


options :: [OptDescr Flag]
options = [Option "a" [] (NoArg ShowAll) "Show all",
           Option "b" [] (NoArg ShowBrief) "Show brief",
           Option "o" [] (NoArg ShowOrd) "Show in order"]


runCommandM :: String -> [Flag] -> [String] -> ReplM ()
runCommandM "def" opts (name:nonOpts) =
  do fs <- fs <$> get
     case FileSystem.lookupDefinition fs name of
       Nothing -> liftIO $ putStrLn $ "definition " ++ show name ++ " does not exist"
       Just def -> putDefinitionM def

runCommandM "def" _ _ =
  liftIO (putStrLn ":def <name>")

runCommandM "show" opts ("namespace":nonOpts) =
    let
        showAll = ShowAll `elem` opts
        showBrief = ShowBrief `elem` opts
        showOrd = ShowOrd `elem` opts
        filename | null nonOpts = ""
                 | otherwise = last nonOpts
    in
      if not showAll && null filename then
          liftIO $ putStrLn ":show namespace [-b] [-o] [-a | <namespace>]"
      else
          showModuleM showAll showBrief showOrd filename

runCommandM "show" _ ["tokens"] =
    liftIO $ putStrLn ":show tokens <namespace>"

runCommandM "show" _ ("tokens":nonOpts) =
    showTokensM $ last nonOpts

runCommandM "show" opts ("renamed":nonOpts) =
    let
        showAll = ShowAll `elem` opts
        filename | null nonOpts = ""
                 | otherwise = last nonOpts
    in
      if not showAll && null filename then
          liftIO $ putStrLn ":show renamed [-a | <namespace>]"
      else
          showRenamedM showAll filename

runCommandM "show" _ _ =
    liftIO $ putStrLn ":show [ namespace | tokens | renamed ]"

runCommandM "load" _ [] =
    liftIO $ putStrLn ":load [ <namespace> | <file/namespace> ]"

runCommandM "load" _ nonOpts =
    do fs <- initialFs <$> get
       liftIO (importFile fs (last nonOpts)) >>= put

runCommandM "l" opts nonOpts =
    runCommandM "load" opts nonOpts

runCommandM _ _ _ =
    liftIO $ putStrLn ":def | :show | :load"


dispatchCommandM :: String -> ReplM ()
dispatchCommandM ln =
    case getOpt Permute options (split ' ' ln) of
      (opts, [], []) -> runCommandM "" opts []
      (opts, nonOpts, []) -> runCommandM (head nonOpts) opts (tail nonOpts)
      (_, _, errs) -> error $ intercalate "\n" errs


promptM :: String -> ReplM ()
promptM ln =
    do liftIO $ do
         addHistory ln
         putLine ln
       process ln
    where process (':':ln) = dispatchCommandM ln
          process ln = runSnippetM ln


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