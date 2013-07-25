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
import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem
import Data.Maybe
import Data.SrcFile (SrcFile(..))
import qualified Data.SrcFile as SrcFile
import Data.Stx
import Data.Symbol
import Data.Type
import Expander (expand, expandDefinition)
import Interpreter
import Lexer
import Linker
import Loader
import Monad.InterpreterM
import Parser
import Printer.PrettyStx
-- edit: remove this hiding
import Renamer (rename, renameDefinition)
import Reorderer (reorder)
import Typechecker
import Utils


data ReplState =
    ReplState { initialFs :: FileSystem, fs :: FileSystem }


type ReplM a = StateT ReplState IO a


doPutLine = False
doPutTokens = True
doPutParsedStx = True
doPutExpr = True
doPutExprT = True
doPutEnvironment = False


putLine :: String -> IO ()
putLine str =
    when doPutLine $
      putStrLn $ "> " ++ str


putParsedStx :: Stx String -> IO ()
putParsedStx stx =
    when doPutParsedStx $ do
      putStrLn "> Parsed stx"
      prettyPrint stx
      putStrLn ""
      putStrLn ""


putExpr :: Show a => a -> IO ()
putExpr expr =
    when doPutExpr (print expr)


putExprT :: (Show a, Show b) => a -> b -> IO ()
putExprT expr t =
    when doPutExprT $
      putStrLn $ show expr ++ " :: " ++ show t


putEnvironment :: Show a => a -> IO ()
putEnvironment env =
    when doPutEnvironment $ do
      putStrLn ""
      putStrLn "Environment"
      print env
      putStrLn ""


parserEither :: Either String a -> a
parserEither fn = either throwParserException id fn


expanderEither :: Either String a -> a
expanderEither fn = either throwExpanderException id fn


renamerEither :: Either String a -> a
renamerEither fn = either throwRenamerException id fn


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
                 putStr "renamed, "

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


interpretM :: Maybe Type -> FileSystem -> SrcFile -> ReplM SrcFile
interpretM mt fs srcfile =
    do let (srcfile', expr) = interpretInteractive fs srcfile
       liftIO $ putExprM mt expr
       return srcfile'
    where putExprM Nothing expr = putExpr expr
          putExprM (Just t) expr = putExprT expr t

          interpretInteractive :: a -> b -> (SrcFile, Expr)
          interpretInteractive = undefined


typecheckAndInterpretM :: FileSystem -> SrcFile -> ReplM SrcFile
typecheckAndInterpretM fs srcfile =
    case typecheckInteractive fs srcfile of
      Left str -> throwTypecheckerException str
      Right (srcfile', t) -> interpretM (Just t) fs srcfile'
    where typecheckInteractive = undefined


mkSnippet :: FileSystem -> Stx String -> Definition
mkSnippet fs stx@(DefnStx _ _ name _) =
    let
        name' = SrcFile.interactiveName ++ "." ++ name
        unprefixed = map SrcFile.name (FileSystem.toAscList fs)
    in
      (Definition.initial name') { unprefixedUses = unprefixed
                                 , srcStx = Just stx }

mkSnippet fs stx =
    mkSnippet fs $ DefnStx Nothing NrDef "val" stx


renameSnippet :: FileSystem -> Definition -> Either String Definition
renameSnippet fs def = renameDefinition fs SrcFile.interactiveName def


runSnippetM :: String -> ReplM ()
runSnippetM ln =
    do fs <- fs <$> get
       let stx = parserEither (parseRepl SrcFile.interactiveName ln)
       liftIO (putParsedStx stx)
       let def = mkSnippet fs stx
           expDef = expanderEither (expandDefinition fs def)
           renDef = renamerEither (renameSnippet fs expDef)
       typDef <- typecheckerEither (typecheckDefinitionM fs renDef)
       let evalDef = interpretDefinition fs typDef
           interactive = FileSystem.get fs SrcFile.interactiveName
           interactive' = SrcFile.updateDefinitions interactive [evalDef]
       modify $ \s -> s { fs = FileSystem.add fs interactive' }
       liftIO $ putExprM (Definition.typ evalDef) (Definition.expr evalDef)
    where putExprM Nothing expr = putExpr expr
          putExprM (Just t) expr = putExprT expr t


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
    liftIO $ putStrLn ":show | :load"


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