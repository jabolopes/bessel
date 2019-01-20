{-# LANGUAGE LambdaCase, MultiWayIf #-}
module Repl where

import Prelude hiding (lex, mod)

import Control.Monad.State
import qualified Data.Char as Char (isSpace)
import Data.Functor ((<$>))
import Data.IORef
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
import Data.Name (Name)
import qualified Data.Name as Name
import qualified Data.PrettyString as PrettyString
import Monad.InterpreterM (Val(..))
import qualified Pretty.Data.Definition as Pretty
import qualified Pretty.Data.Module as Pretty
import qualified Pretty.Repl as Pretty
import Data.Result (Result(..))
import qualified Stage
import qualified Stage.Loader as Loader (preload)
import qualified Utils (split)

data ReplState =
    ReplState { initialFs :: FileSystem, fs :: FileSystem }

type ReplM a = StateT ReplState IO a

putVal :: Either String (IORef Val) -> IO ()
putVal (Left err) = putStrLn err
putVal (Right ref) = putVal' =<< readIORef ref
  where
    putVal' (IOVal m) = putVal' =<< m
    putVal' val = print val

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
             putStrLn . show $ Module.modName mod
             res <- Stage.stageModule fs mod
             case res of
               Right (fs', _) -> loop fs' mods is
               Left err ->
                 do putStrLn (PrettyString.toString err)
                    loop fs mods is

importFile :: FileSystem -> Name -> IO ReplState
importFile fs modName =
  Loader.preload fs modName >>=
    \case
      Left err -> throwLoaderException err
      Right mods ->
        do fs' <- stageFiles mods
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
           case FileSystem.lookup fs $ Name.untyped filename of
             Nothing -> liftIO $ do
                          putStrLn $ "module " ++ show filename ++ " has not been staged"
                          putStrLn $ "staged modules are " ++ intercalate ", " (map (show . Module.modName) (FileSystem.toAscList fs))
                          return []
             Just mod -> return [mod]
  in
    do mods <- filesM
       let modDoc = Pretty.docModules showBrief showOrd showFree showSrc showExp showRen mods
       liftIO $ putStrLn $ PrettyString.toString modDoc

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
  | showHelp = usageM
  | isDefinitionName nonOpts =
      do let moduleName = last $ init nonOpts
             definitionName = last nonOpts
         fs <- fs <$> get
         liftIO . putStrLn . PrettyString.toString $
           case FileSystem.lookupDefinition fs . Name.untyped $ moduleName ++ "." ++ definitionName of
             Bad err -> err
             Ok def -> Pretty.docDefn showFree showSrc showExp showRen def
  | showAll || isModuleName nonOpts =
      do let moduleName
               | null nonOpts = ""
               | otherwise = last nonOpts
         showMeM showAll showBrief showOrd showFree showSrc showExp showRen moduleName
  | null nonOpts =
      do fs <- fs <$> get
         let moduleNames = map Module.modName . FileSystem.toAscList $ fs
         liftIO . putStrLn . PrettyString.toString . Pretty.docModuleNames $ moduleNames
  | otherwise = usageM
  where
    showAll = ShowAll `elem` opts
    showBrief = ShowBrief `elem` opts
    showHelp = ShowHelp `elem` opts
    showOrd = ShowOrd `elem` opts
    showFree = ShowFree `elem` opts
    showSrc = ShowSrc `elem` opts
    showExp = ShowExp `elem` opts
    showRen = ShowRen `elem` opts

    isDefinitionName (_:_:_) = True
    isDefinitionName _ = False

    isModuleName (_:_) = True
    isModuleName _ = False

    usageM =
      liftIO . putStr $ usageInfo "def [-b] [--help] [--free] [--src] [--exp] [--ren] [-o] [-a | <me>]" options
runCommandM "load" _ nonOpts
  | null nonOpts =
    liftIO $ putStrLn ":load [ <me> | <file/me> ]"
  | otherwise =
    do fs <- initialFs <$> get
       liftIO (importFile fs (Name.untyped (last nonOpts))) >>= put
runCommandM "l" opts nonOpts =
  runCommandM "load" opts nonOpts
runCommandM _ _ _ =
  liftIO $ putStrLn ":def | :load | :me"

dispatchCommandM :: String -> ReplM ()
dispatchCommandM ln =
    case getOpt Permute options (Utils.split ' ' ln) of
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
                 | all Char.isSpace ln -> replM
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
