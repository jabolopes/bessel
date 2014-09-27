module Repl where

import Prelude hiding (lex, mod)

import Control.Monad.State
import Data.Char (isSpace)
import Data.Functor ((<$>))
import Data.List (intercalate)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo)
import System.Console.Readline
import System.IO (hFlush, stdout)
import System.IO.Error

import qualified Data.Definition as Definition
import Data.Exception
import Data.FileSystem (FileSystem)
import qualified Data.FileSystem as FileSystem
import Data.Module (Module)
import qualified Data.Module as Module
import qualified Data.PrettyString as PrettyString
import Lexer (lexTokens)
import Loader (preload, readFileM)
import Monad.InterpreterM (Val(..))
import qualified Pretty.Data.Definition as Pretty
import qualified Pretty.Data.Module as Pretty
import qualified Pretty.Repl as Pretty
import qualified Stage
import Utils (split)

data ReplState =
    ReplState { initialFs :: FileSystem, fs :: FileSystem }

type ReplM a = StateT ReplState IO a

putVal :: Either String Val -> IO ()
putVal (Left err) = putStrLn err
putVal (Right (IOVal m)) =
  do val <- m
     print val
putVal (Right val) = print val

stageFiles :: [Module] -> IO FileSystem
stageFiles mods =
  do putStrLn $ "Staging " ++ show n ++ " modules"
     loop FileSystem.empty mods [(1 :: Int)..]
  where n = length mods

        putHeader i =
          putStr $ "[" ++ show i ++ "/" ++ show n ++ "] "

        loop fs [] _ = return fs
        loop fs (mod:mods) (i:is) =
          do putHeader i
             putStrLn (Module.modName mod)
             res <- Stage.stageModule fs mod
             case res of
               Right (fs', _) -> loop fs' mods is
               Left err ->
                 do putStrLn (PrettyString.toString err)
                    loop fs mods is

importFile :: FileSystem -> String -> IO ReplState
importFile fs filename =
  do mods <- preload fs filename
     fs' <- stageFiles mods
     let interactive = Module.mkInteractiveModule mods []
         fs'' = FileSystem.add fs' interactive
     return ReplState { initialFs = fs, fs = fs'' }

runSnippetM :: String -> ReplM ()
runSnippetM ln =
  do fs <- fs <$> get
     res <- liftIO $ Stage.stageDefinition fs ln
     case res of
       Left err -> liftIO . putStrLn . PrettyString.toString $ err
       Right (fs', defs) ->
         do modify $ \s -> s { fs = fs' }
            liftIO $ putVal (Definition.defVal (last defs))

showMeM :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> String -> StateT ReplState IO ()
showMeM showAll showBrief showOrd showFree showSrc showExp showRen filename =
  let
    filesM
      | showAll = FileSystem.toAscList . fs <$> get
      | otherwise =
        do fs <- fs <$> get
           case FileSystem.lookup fs filename of
             Nothing -> liftIO $ do
                          putStrLn $ "module " ++ show filename ++ " has not been staged"
                          putStrLn $ "staged modules are " ++ intercalate ", " (map Module.modName (FileSystem.toAscList fs))
                          return []
             Just mod -> return [mod]
  in
    do mods <- filesM
       let modDoc = Pretty.docModules showBrief showOrd showFree showSrc showExp showRen mods
       liftIO $ putStrLn $ PrettyString.toString modDoc

showTokensM :: String -> ReplM ()
showTokensM filename =
    liftIO (print . lexTokens filename =<< readFileM filename)

data Flag
  = ShowAll
  | ShowBrief
  | ShowHelp
  | ShowOrd

  | ShowFree
  | ShowSrc
  | ShowExp
  | ShowRen
    deriving (Eq, Show)

options :: [OptDescr Flag]
options = [Option "a" [] (NoArg ShowAll) "Show all",
           Option "b" [] (NoArg ShowBrief) "Show brief",
           Option "" ["help"] (NoArg ShowHelp) "Show help",
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
      if showHelp then
        usageM
      else if not showAll && null me then
             showModuleNamesM
           else
             showMeM showAll showBrief showOrd showFree showSrc showExp showRen me
  | otherwise =
    do let name = last nonOpts
       fs <- fs <$> get
       case FileSystem.lookupDefinition fs name of
         Nothing -> liftIO $ putStrLn $ "definition " ++ show name ++ " does not exist"
         Just def -> liftIO $ putStrLn $ PrettyString.toString $ Pretty.docDefn showFree showSrc showExp showRen def
  where showAll = ShowAll `elem` opts
        showBrief = ShowBrief `elem` opts
        showHelp = ShowHelp `elem` opts
        showOrd = ShowOrd `elem` opts
        showFree = ShowFree `elem` opts
        showSrc = ShowSrc `elem` opts
        showExp = ShowExp `elem` opts
        showRen = ShowRen `elem` opts

        usageM =
          liftIO $ putStr $ usageInfo "def [-b] [--help] [--free] [--src] [--exp] [--ren] [-o] [-a | <me>]" options

        showModuleNamesM =
          do fs <- fs <$> get
             let modNames = map Module.modName . FileSystem.toAscList $ fs
             liftIO . putStrLn . PrettyString.toString . Pretty.docModuleNames $ modNames
runCommandM "load" _ nonOpts
  | null nonOpts =
    liftIO $ putStrLn ":load [ <me> | <file/me> ]"
  | otherwise =
    do fs <- initialFs <$> get
       liftIO (importFile fs (last nonOpts)) >>= put
runCommandM "l" opts nonOpts =
  runCommandM "load" opts nonOpts
runCommandM _ _ _ =
  liftIO $ putStrLn ":def | :load | :me"

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
       liftIO (hFlush stdout)
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
putUserException (LoaderException err) =
  putStrLn . PrettyString.toString $
  PrettyString.text "loader error: "
  PrettyString.$+$
  PrettyString.nest err
putUserException (InterpreterException str) =
  putStrLn $ "interpreter error: " ++ str
putUserException (LexerException str) =
  putStrLn $ "lexical error: " ++ str
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
