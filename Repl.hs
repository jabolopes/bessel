{-# LANGUAGE BangPatterns #-}
module Repl where

import Prelude hiding (lex)

import Control.Monad.State
import Data.Char (isSpace)
import Data.Functor ((<$>))
import Data.List (intercalate, isPrefixOf, nub)
import Data.Map (Map)
import qualified Data.Map as Map (elems, null, union)
import System.Console.GetOpt
import System.Console.Readline
import System.IO.Error

import Config
import qualified Data.Env as Env (getBinds)
import Data.Exception
import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem
import Data.Maybe
import Data.SrcFile
import qualified Data.SrcFile as SrcFile
import Data.Stx
import Data.Symbol
import Data.Type
import Interpreter
import Lexer
import Linker
import Loader
import Monad.InterpreterM
import Parser
import Printer.PrettyStx
-- edit: remove this hiding
import Renamer hiding (fs)
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

          renameFile fs srcfile =
              updateFs fs $ renamerEither $ rename fs srcfile

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

                 let (renfs, rens) = renameFile fs srcs
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


typecheckAndInterpretM :: FileSystem -> SrcFile -> ReplM SrcFile
typecheckAndInterpretM fs srcfile =
    case typecheckInteractive fs srcfile of
      Left str -> throwTypecheckerException str
      Right (srcfile', t) -> interpretM (Just t) fs srcfile'


runSnippetM :: String -> ReplM ()
runSnippetM ln =
    do let stx = parserEither (parseRepl interactiveName ln)
       liftIO (putParsedStx stx)

       fs <- fs <$> get

       let srcf = (SrcFile.mkInteractiveSrcFile (FileSystem.toAscList fs) [stx]) { name = "Repl" }
           renf = renamerEither (rename fs srcf)
       evalf <- typecheckAndInterpretM fs renf

       modify $ \s -> s { fs = mergeReplInteractive fs evalf }
    where mergeReplInteractive fs srcfile =
              let
                  interactive = FileSystem.get fs interactiveName
                  interactive' = interactive { defs = defs srcfile `Map.union` defs interactive }
              in
                FileSystem.add fs interactive'


showModuleM :: Bool -> Bool -> String -> ReplM ()
showModuleM showAll showBrief filename =
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
              | null (deps srcfile) = ""
              | otherwise = " uses " ++ intercalate ", " (SrcFile.deps srcfile)

          showDefns srcfile
              | Map.null (defs srcfile) = "\n no definitions"
              | otherwise = "\n " ++ intercalate "\n " (map show $ Map.elems $ SrcFile.defs srcfile)

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
  do ensureLoadedM filename
     let filesM
           | showAll = FileSystem.toAscList . fs <$> get
           | otherwise =
                 do fs <- fs <$> get
                    return $ (:[]) $ FileSystem.get fs filename
     filesM >>= showFiles
    where showFiles [] = return ()
          showFiles (srcfile:srcfiles) =
              do liftIO $ prettyPrintSrcFile srcfile
                 showFiles srcfiles

          -- edit: check if file has changed on disk
          ensureLoadedM filename =
            do fs <- fs <$> get
               when (isNothing (FileSystem.lookup fs filename)) $
                 liftIO (importFile fs filename) >>= put


data Flag
    = ShowAll
    | ShowBrief
      deriving (Eq, Show)


options :: [OptDescr Flag]
options = [Option "a" [] (NoArg ShowAll) "Show all",
           Option "b" [] (NoArg ShowBrief) "Show brief"]


runCommandM :: String -> [Flag] -> [String] -> ReplM ()
runCommandM "show" opts ("namespace":nonOpts) =
    let
        showAll = ShowAll `elem` opts
        showBrief = ShowBrief `elem` opts
        filename | null nonOpts = ""
                 | otherwise = last nonOpts
    in
      if not showAll && null nonOpts then
          liftIO $ putStrLn ":show namespace [-b] [-a | <namespace>]"
      else
          showModuleM showAll showBrief filename

runCommandM "show" _ ["tokens"] =
    liftIO $ putStrLn ":show tokens <namespace>"

runCommandM "show" _ ("tokens":nonOpts) =
    showTokensM $ last nonOpts

runCommandM "show" _ ["renamed"] =
    liftIO $ putStrLn ":show renamed [-a] <namespace>"

runCommandM "show" opts ("renamed":nonOpts) =
    let showAll = ShowAll `elem` opts in
    showRenamedM showAll $ last nonOpts

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