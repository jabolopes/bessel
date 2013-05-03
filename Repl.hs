{-# LANGUAGE BangPatterns #-}
module Repl where

import Prelude hiding (lex, catch)

import Control.Monad.State
import Data.Char (isSpace)
import Data.Functor ((<$>))
import Data.List (intercalate, isPrefixOf, nub)
import Data.Map (Map)
import qualified Data.Map as Map ((!), insert, elems, empty, keys, lookup, toList)
import System.Console.GetOpt
import System.Console.Readline
import System.IO.Error (catchIOError)

import Config
import qualified Data.Env as Env (getBinds)
import Data.Exception
import Data.Maybe
import Data.SrcFile
import qualified Data.SrcFile as SrcFile
import Data.Stx
import Data.Type
import Interpreter
import Lexer
import Loader
import Monad.InterpreterM
import Parser
import Printer.PrettyStx
-- edit: remove this hiding
import Renamer hiding (fs)
import Typechecker
import Utils


data ReplState =
    ReplState { initialFs :: Map String SrcFile
              , fs :: Map String SrcFile
              , interactive :: SrcFile }


type ReplM a = StateT ReplState IO a


doPutLine = False
doPutTokens = True
doPutParsedStx = True
doPutExpr = True
doPutExprT = True
doPutEnvironment = False


putLine :: String -> IO ()
putLine str =
    when doPutLine (putStrLn $ "> " ++ str)


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


renamerEither :: Monad m => Either String a -> m a
renamerEither fn = return $ either throwRenamerException id fn


typecheckerEither :: Monad m => Either String a -> m a
typecheckerEither fn = return $ either throwTypecheckerException id fn


stageFiles :: [SrcFile] -> IO (Map String SrcFile)
stageFiles srcfiles =
    do liftIO $ putStrLn $ "Staging " ++ show n ++ " namespaces"
       loop Map.empty srcfiles [1..]
    where n = length srcfiles

          putHeader i =
              putStr $ "[" ++ show i ++ "/" ++ show n ++ "] "

          updateFs fs srcfile =
              Map.insert (SrcFile.name srcfile) srcfile fs

          loop fs [] _ = return fs
          loop fs (srcs:srcss) (i:is) =
              do putHeader i
                 putStr $ SrcFile.name srcs ++ ": "

                 !rens <- renamerEither (rename fs srcs)
                 let renfs = updateFs fs rens
                 putStr "renamed, "

                 !typs <- typecheckerEither (typecheck renfs rens)
                 let typfs = updateFs renfs typs
                 putStr "typechecked, "

                 let evals = interpret typfs typs
                     evalfs = updateFs typfs evals
                 putStrLn "evaluated"

                 loop evalfs srcss is


importFile :: Map String SrcFile -> String -> IO ReplState
importFile fs filename =
    do srcfiles <- preload fs filename
       fs' <- stageFiles srcfiles
       let interactive = mkInteractiveSrcFile $ map SrcFile.name srcfiles
       return ReplState { initialFs = fs, fs = fs', interactive = interactive }


interpretM :: Maybe Type -> Map String SrcFile -> SrcFile -> ReplM SrcFile
interpretM mt fs srcfile =
    do let (srcfile', expr) = interpretInteractive fs srcfile
       liftIO $ putExprM mt expr
       return srcfile'
    where putExprM Nothing expr = putExpr expr
          putExprM (Just t) expr = putExprT expr t


typecheckM :: Map String SrcFile -> SrcFile -> ReplM SrcFile
typecheckM fs srcfile =
    case typecheckInteractive fs srcfile of
      Left str -> throwTypecheckerException str
      Right (srcfile', t) -> interpretM (Just t) fs srcfile'


runSnippetM :: String -> ReplM ()
runSnippetM ln =
    do state <- get
       liftIO (putLine ln)

       let stx = parseRepl "interactive" ln
       liftIO (putParsedStx stx)

       fs <- fs <$> get
       interactive <- putStxInInteractive stx . interactive <$> get
       interactive' <- renamerEither (rename fs interactive)
       interactive'' <- typecheckM fs interactive'
       put state { interactive = interactive'' }
    where putStxInInteractive stx srcfile =
            let Just (Namespace uses _) = SrcFile.srcNs srcfile in
            srcfile { srcNs = Just (Namespace uses [stx]) }


showModuleM :: Bool -> Bool -> String -> ReplM ()
showModuleM showAll showBrief filename =
    let
        filesM
            | filename == "Interactive" =
                do interactive <- interactive <$> get
                   return [interactive]
            | showAll =
                Map.elems . fs <$> get
            | otherwise =
                do fs <- fs <$> get
                   case Map.lookup filename fs of
                     Nothing -> do liftIO $ do
                                     putStrLn $ "namespace " ++ show filename ++ " has not been staged"
                                     putStrLn $ "staged namespaces are " ++ intercalate ", " (map SrcFile.name (Map.elems fs))
                                   return []
                     Just srcfile -> return [srcfile]
    in
      filesM >>= (liftIO . mapM_ (putStrLn . showSrcFile))
    where showDeps srcfile = 
              "\n\n\t deps = " ++ intercalate ('\n':replicate 16 ' ') (SrcFile.deps srcfile)

          showDefs srcfile =
              "\n\n\t defs = " ++ intercalate spacer [ showTuple (x, e x, t x) | x <- Map.keys (SrcFile.symbols srcfile) ]
              where spacer = '\n':replicate 16 ' '
                    e name = Map.lookup name (SrcFile.exprs srcfile)
                    t name = SrcFile.ts srcfile Map.! name
                    showTuple (x, Nothing, y) = x ++ " :: " ++ show y
                    showTuple (x, Just y, z) = x ++ " = " ++ show y ++ " :: " ++ show z

          showSrcNs SrcFile { t = CoreT } = ""

          showSrcNs SrcFile { t = InteractiveT, srcNs = Just (Namespace uses [stx]) } =
              "\n\n\t srcNs = " ++ intercalate ('\n':replicate 17 ' ') (map use uses) ++
              "\n\n\t         " ++ show stx
              where use (x, "") = "use " ++ x
                    use (x, y) = "use " ++ x ++ " as " ++ y

          showSrcNs SrcFile { srcNs = Just (Namespace uses _) } =
              "\n\n\t srcNs = " ++ intercalate ('\n':replicate 17 ' ') (map use uses)
              where use (x, "") = "use " ++ x
                    use (x, y) = "use " ++ x ++ " as " ++ y

          showRenNs SrcFile { renNs = Nothing } =
              "\n\n\t renNs = Nothing"

          showRenNs SrcFile { renNs = Just (Namespace uses _) } =
              "\n\n\t renNs = " ++ intercalate ('\n':replicate 17 ' ') (map use uses)
              where use (x, "") = "use " ++ x
                    use (x, y) = "use " ++ x ++ " as " ++ y

          showSrcFile srcfile
              | showBrief = "SrcFile " ++ show (SrcFile.name srcfile) ++ " " ++ show (SrcFile.deps srcfile) ++ " ..."
              | otherwise = SrcFile.name srcfile ++ " (" ++ show (SrcFile.t srcfile) ++ ")" ++
                            showDeps srcfile ++
                            showDefs srcfile ++
                            -- showSymbols srcfile ++
                            -- showTs srcfile ++
                            -- showExprs srcfile ++
                            showSrcNs srcfile ++
                            showRenNs srcfile


showTokensM :: String -> ReplM ()
showTokensM filename =
    liftIO (print . lexTokens filename =<< readFileM filename)


showRenamedM :: Bool -> String -> ReplM ()
showRenamedM showAll filename =
  do ensureLoadedM filename
     let filesM
           | filename == "Interactive" =
             do interactive <- interactive <$> get
                return [interactive]
           | showAll =
               Map.elems . fs <$> get
           | otherwise =
                 do fs <- fs <$> get
                    return $ (:[]) $ fromJust $ Map.lookup filename fs
     filesM >>= showFiles
    where showFiles [] = return ()
          showFiles (srcfile:srcfiles) =
              do liftIO $ prettyPrintSrcFile srcfile
                 showFiles srcfiles

          -- edit: check if file has changed on disk
          ensureLoadedM filename =
            do fs <- fs <$> get
               when (isNothing (Map.lookup filename fs)) $
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


finallyIOException :: Show a => ReplState -> a -> IO (Bool, ReplState)
finallyIOException state e =
    print e >> return (False, state)


finallyFlException :: a -> FlException -> IO (Bool, a)
finallyFlException state e =
    flException e >> return (False, state)


flException :: FlException -> IO ()
flException (RenamerException str) =
    putStrLn $ "renamer error: " ++ str

flException (TypecheckerException str) =
    putStrLn $ "typechecker error: " ++ str

flException (InterpreterException str) =
    putStrLn $ "interpreter error: " ++ str

flException (LexException str) =
    putStrLn $ "lexical error: " ++ str

flException (ParseException str) =
    putStrLn $ "parse error: " ++ str

flException (SignalException str) =
    putStrLn $ "uncaught exception: " ++ str


repl :: ReplState -> IO ()
repl state =
    do (b, state') <- (runStateT replM state
                       `catchIOError` finallyIOException state)
                      `catchFlException` finallyFlException state
       unless b (repl state')


batch :: String -> ReplState -> IO ()
batch ln state =
    do _ <- (runStateT (promptM ln >> return True) state
             `catchIOError` finallyIOException state)
            `catchFlException` finallyFlException state
       return ()