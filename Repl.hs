{-# LANGUAGE BangPatterns #-}
module Repl where

import Prelude hiding (lex)

import Control.Monad.State
import Data.Char (isSpace)
import Data.Functor ((<$>))
import Data.List (intercalate)
import qualified Data.Map as Map (elems, null)
import Data.Maybe (fromJust, isNothing)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo)
import System.Console.Readline
import System.IO.Error

import Data.Definition (Definition(..))
import qualified Data.Definition as Definition
import Data.Exception
import Data.Expr (DefnKw(..), Expr(..))
import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem
import Data.SrcFile (SrcFile)
import qualified Data.SrcFile as SrcFile hiding (unprefixedUses)
import qualified Doc.Doc as Doc
import qualified Doc.Definition as Doc
import qualified Doc.Expr as Doc
import Expander (expand, expandDefinition)
import Interpreter (interpret, interpretDefinition)
import Lexer (lexTokens)
import Loader (preload, readFileM)
import Monad.InterpreterM (Val)
import Parser (parseRepl)
import Printer.PrettyExpr (prettyPrintSrcFile, prettyPrint)
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


stageFiles :: [SrcFile] -> IO FileSystem
stageFiles srcfiles =
    do putStrLn $ "Staging " ++ show n ++ " modules"
       loop FileSystem.empty srcfiles [1..]
    where n = length srcfiles

          putHeader i =
              putStr $ "[" ++ show i ++ "/" ++ show n ++ "] "

          updateFs fs !srcfile =
              (FileSystem.add fs srcfile, srcfile)

          reorderFile fs = updateFs fs . reorder
          expandFile fs = updateFs fs . expanderEither . expand fs
          renameFile fs = updateFs fs . renamerEither . rename fs

          interpretFile fs srcfile =
              let srcfile' = interpret fs srcfile in
              (FileSystem.add fs srcfile', srcfile')

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

                 let (evalfs, evals) = interpretFile renfs rens
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
     let def = mkSnippet fs expr
         expDef = expanderEither (expandDefinition fs def)
         renDef = renamerEither (renameSnippet fs expDef)
         evalDef = interpretDefinition fs renDef
         interactive = FileSystem.get fs SrcFile.interactiveName
         interactive' = SrcFile.updateDefinitions interactive [evalDef]
     modify $ \s -> s { fs = FileSystem.add fs interactive' }
     liftIO $ putVal (Definition.val evalDef)


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
                                     putStrLn $ "staged modules are " ++ intercalate ", " (map SrcFile.name (FileSystem.toAscList fs))
                                   return []
                     Just srcfile -> return [srcfile]
    in
      mapM_ putSrcFile =<< filesM
    where showDeps srcfile
              | null (SrcFile.deps srcfile) = ""
              | otherwise = " uses " ++ intercalate ", " (SrcFile.deps srcfile)

          putDefns srcfile
            | Map.null (SrcFile.defs srcfile) =
              liftIO (putStrLn " no definitions")
            | otherwise =
              mapM_ (liftIO . putStrLn . showDefinition showFree showSrc showExp showRen) defns
            where defns
                    | showOrd = SrcFile.defsAsc $ srcfile
                    | otherwise = Map.elems . SrcFile.defs $ srcfile

          putSrcFile srcfile
            | showBrief = liftIO (putStrLn str)
            | otherwise =
              do liftIO (putStrLn str)
                 putDefns srcfile
            where str = SrcFile.name srcfile ++ " (" ++ show (SrcFile.t srcfile) ++ ")" ++ showDeps srcfile

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
                                     putStrLn $ "module " ++ show filename ++ " has not been staged"
                                     putStrLn $ "staged modules are " ++ intercalate ", " (map SrcFile.name (FileSystem.toAscList fs))
                                   return []
                     Just srcfile -> return [srcfile]
    in
      filesM >>= liftIO . mapM_ prettyPrintSrcFile

isLeft (Left _) = True
isLeft _ = False

fromLeft (Left x) = x

showDefinition :: Bool -> Bool -> Bool -> Bool -> Definition -> String
showDefinition showFree showSrc showExp showRen def
  | isLeft (Definition.val def) =
    Doc.definitionError n (fromLeft (Definition.val def))
  | isNothing (Definition.srcExpr def) =
    Doc.definitionError n "definition is missing source expression"
  | isNothing (Definition.expExpr def) =
    Doc.definitionError n "definition has not expanded expression"
  | isLeft (Definition.renExpr def) =
    Doc.definitionError n (fromLeft (Definition.renExpr def))
  | otherwise =
    Doc.definitionOk (Definition.name def)
                     (defVal (Definition.val def))
                     free
                     (src def)
                     (exp def)
                     (ren def)
  where n = Definition.name def

        defVal (Right x) = show x

        free
          | showFree = Definition.freeNames def
          | otherwise = []
        
        src Definition { srcExpr = Just expr }
          | showSrc = Doc.docExpr Doc.SrcDocT expr
          | otherwise = Doc.empty

        exp Definition { expExpr = Just expr }
          | showExp = Doc.docExpr Doc.ExpDocT expr
          | otherwise = Doc.empty

        ren Definition { renExpr = Right expr }
          | showRen = Doc.docExpr Doc.RenDocT expr
          | otherwise = Doc.empty

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
         Just def -> liftIO $ putStrLn $ showDefinition showFree showSrc showExp showRen def
  where showAll = ShowAll `elem` opts
        showBrief = ShowBrief `elem` opts
        showOrd = ShowOrd `elem` opts
        showFree = ShowFree `elem` opts
        showSrc = ShowSrc `elem` opts
        showExp = ShowExp `elem` opts
        showRen = ShowRen `elem` opts

        usageM =
          liftIO $ putStrLn $ usageInfo "def [-b] [--free] [--src] [--exp] [--ren] [-o] [-a | <me>]" options

runCommandM "show" _ ["tokens"] =
    liftIO $ putStrLn ":show tokens <me>"

runCommandM "show" _ ("tokens":nonOpts) =
    showTokensM $ last nonOpts

runCommandM "show" opts ("renamed":nonOpts) =
  let
    showAll = ShowAll `elem` opts
    filename | null nonOpts = ""
             | otherwise = last nonOpts
  in
    if not showAll && null filename then
      liftIO $ putStrLn ":show renamed [-a | <me>]"
    else
      showRenamedM showAll filename

runCommandM "show" _ _ =
    liftIO $ putStrLn ":show [ tokens | renamed ]"

runCommandM "load" _ [] =
    liftIO $ putStrLn ":load [ <me> | <file/me> ]"

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
