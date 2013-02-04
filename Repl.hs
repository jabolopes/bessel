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

--import System.IO.Error (catchIOError)
import System.IO.Error (catch)

import Config
import qualified Data.Env as Env (getBinds)
import Data.Exception
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
    ReplState { fs :: Map String SrcFile
              , interactive :: SrcFile }

type ReplM a = StateT ReplState IO a


catchIOError :: IO a -> (IOError -> IO a) -> IO a
catchIOError = catch


doPutLine = False
doPutTokens = True
doPutParsedStx = True
doPutRenamedStx = False
doPutExpr = True
doPutExprT = True
doPutEnvironment = False


putLine :: String -> IO ()
putLine str
    | doPutLine = putStrLn $ "> " ++ str
    | otherwise = return ()


putTokens :: [Token] -> IO ()
putTokens tokens
    | doPutTokens =
        do putStrLn "> Tokens"
           mapM_ (putStrLn . show) tokens
           putStrLn ""
    | otherwise = return ()


putParsedStx :: Stx String -> IO ()
putParsedStx stx
    | doPutParsedStx =
        do putStrLn "> Parsed stx"
           prettyPrint stx
           putStrLn ""
           putStrLn ""
    | otherwise = return ()


putRenamedStx :: Stx String -> IO ()
putRenamedStx stx
    | doPutRenamedStx =
        do putStrLn "> Renamed stx"
           prettyPrint stx
           putStrLn ""
           putStrLn ""
    | otherwise = return ()


putExpr :: Show a => a -> IO ()
putExpr expr
    | doPutExpr = putStrLn $ show expr
    | otherwise = return ()


putExprT :: (Show a, Show b) => a -> b -> IO ()
putExprT expr t
    | doPutExprT = putStrLn $ show expr ++ " :: " ++ show t
    | otherwise = return ()


putEnvironment :: Show a => a -> IO ()
putEnvironment env
    | doPutEnvironment =
        do putStrLn ""
           putStrLn "Environment"
           putStrLn $ show env
           putStrLn ""
    | otherwise = return ()


renamerEither :: Monad m => Either String a -> m a
renamerEither fn = return $ either throwRenamerException id fn


typecheckerEither :: Monad m => Either String a -> m a
typecheckerEither fn = return $ either throwTypecheckerException id fn


importFile :: Map String SrcFile -> String -> IO ReplState
importFile fs filename =
    do srcfiles <- preload fs filename
       fs' <- loop Map.empty srcfiles
       let interactiveDeps = map SrcFile.name srcfiles
           interactive = mkInteractiveSrcFile interactiveDeps
       return $ ReplState { fs = fs', interactive = interactive }
    where updateFs fs srcfile = Map.insert (SrcFile.name srcfile) srcfile fs

          loop fs [] = return fs
          loop fs (srcs:srcss) =
              do putStr $ SrcFile.name srcs
                 rens <- renamerEither $ rename fs srcs
                 let renfs = updateFs fs rens
                 putStr ": renamed"
                 typs <- typecheckerEither $ typecheck renfs rens
                 let typfs = updateFs renfs typs
                 putStr ", typechecked"
                 let evals = interpret typfs typs
                     evalfs = updateFs typfs evals
                 putStrLn ", evaluated"
                 loop evalfs srcss


runInterpretM :: SrcFile -> Stx String -> ReplM SrcFile
runInterpretM srcfile stx =
    do let (srcfile', expr) = interpretInteractive srcfile stx
       liftIO $ putExpr expr
       return srcfile'


runTypecheckM :: SrcFile -> Stx String -> ReplM SrcFile
runTypecheckM srcfile stx =
    -- case typecheckInteractive srcfile stx of
    do fs <- fs <$> get
       let corefile = fs Map.! "Core"
       case typecheckInteractive (SrcFile.ts corefile) stx of
         Left str -> throwTypecheckerException str
         Right (srcfile', t) -> do let (srcfile'', expr) = interpretInteractive srcfile stx
                                   liftIO $ putExprT expr t
                                   return srcfile''


runSnippetM :: String -> ReplM ()
runSnippetM ln =
    do state <- get
       let tokens = lex ln
           stx = parseRepl tokens
       fs <- fs <$> get
       interactive <- interactive <$> get
       (interactive', stx') <- renamerEither $ renameInteractive fs interactive stx
       liftIO $ putRenamedStx stx'
       let fn | doTypecheck = runTypecheckM
              | otherwise = runInterpretM
       interactive'' <- fn interactive' stx'
       put $ state { interactive = interactive' }


showModuleM :: Bool -> Bool -> String -> ReplM ()
showModuleM showAll showBrief filename =
    if filename == "Interactive" then
        do interactive <- interactive <$> get
           liftIO $ putStrLn $ showBriefly interactive
    else
        do fs <- fs <$> get
           srcfiles <- liftIO $ preload fs filename
           let srcfiles' | showAll = srcfiles
                         | otherwise = [last srcfiles]
           liftIO $ mapM_ (putStrLn . showBriefly) srcfiles'
    where showDeps srcfile = 
              "\n\n\t deps = " ++ intercalate ", " (SrcFile.deps srcfile)

          showSymbols srcfile =
              "\n\n\t symbols = " ++ intercalate ", " (Map.keys $ SrcFile.symbols srcfile)

          showTs srcfile =
              "\n\n\t ts = " ++ intercalate ('\n':replicate 14 ' ') (map showTuple $ Map.toList $ SrcFile.ts srcfile)
              where showTuple (x, y) =
                        x ++ " :: " ++ show y

          showExprs srcfile =
              "\n\n\t exprs = " ++ intercalate ", " (map fst $ Map.toList $ SrcFile.exprs srcfile)

          showSrcNs SrcFile { srcNs = Left (Namespace uses _) } =
              "\n\n\t srcNs = " ++ intercalate ('\n':replicate 17 ' ') (map use uses)
              where use (x, "") = "use " ++ x
                    use (x, y) = "use " ++ x ++ " as " ++ y

          showSrcNs SrcFile { srcNs = Right _ } =
              "\n\n\t srcNs = <built-in>"

          showRenNs SrcFile { renNs = Nothing } =
              "\n\n\t renNs = Nothing"

          showRenNs SrcFile { renNs = Just (Namespace uses _) } =
              "\n\n\t renNs = " ++ intercalate ", " (map use uses)
              where use (x, "") = "use " ++ x
                    use (x, y) = "use " ++ x ++ " as " ++ y

          showBriefly srcfile
              | showBrief = "SrcFile " ++ show (SrcFile.name srcfile) ++ " " ++ show (SrcFile.deps srcfile) ++ " ..."
              | otherwise = SrcFile.name srcfile ++
                            showDeps srcfile ++
                            showSymbols srcfile ++
                            showTs srcfile ++
                            showExprs srcfile ++
                            showSrcNs srcfile ++
                            showRenNs srcfile


showTokensM :: String -> ReplM ()
showTokensM filename =
    liftIO $ do
      str <- readFileM filename
      putStrLn $ show $ lex str


showRenamedM :: Bool -> String -> ReplM ()
showRenamedM showAll filename =
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
                     Nothing -> return []
                     Just srcfile -> return [srcfile]
    in       
      filesM >>= showFiles
    where showFiles [] = return ()
          showFiles (srcfile:srcfiles) =
              do liftIO $ prettyPrintSrcFile srcfile
                 showFiles srcfiles


data Flag
    = ShowAll
    | ShowBrief
      deriving (Eq, Show)


options :: [OptDescr Flag]
options = [Option "a" [] (NoArg ShowAll) "Show all",
           Option "b" [] (NoArg ShowBrief) "Show brief"]


runCommandM :: String -> [Flag] -> [String] -> ReplM ()
runCommandM "show" opts nonOpts
    | head nonOpts == "module" =
        let showAll = ShowAll `elem` opts
            showBrief = ShowBrief `elem` opts in
        showModuleM showAll showBrief $ last nonOpts

    | head nonOpts == "tokens" =
        showTokensM $ last nonOpts

    | head nonOpts == "renamed" =
        let showAll = ShowAll `elem` opts in
        showRenamedM showAll $ last nonOpts

runCommandM "load" opts nonOpts =
    do fs <- fs <$> get
       liftIO (importFile fs (last nonOpts)) >>= put

runCommandM "l" opts nonOpts =
    runCommandM "load" opts nonOpts


dispatchCommandM :: String -> ReplM ()
dispatchCommandM ln =
    case getOpt Permute options (split ' ' ln) of
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
    do mprompt <- liftIO $ readline "fl$ "
       case mprompt of 
         Nothing -> return True
         Just ln | ln == ":quit" -> return True
                 | all isSpace ln -> replM
                 | otherwise -> promptM ln >> return False


finallyIOException state e =
    ioException e >> return (False, state)

finallyFlException state e =
    flException e >> return (False, state)

ioException e =
    putStrLn $ show e

flException (RenamerException str) =
    putStrLn $ "renamer error: " ++ str

flException (TypecheckerException str) =
    putStrLn $ "typechecker error: " ++ str

flException (InterpreterException str) =
    putStrLn $ "interpreter error: " ++ str

flException (ParseException str) =
    putStrLn $ "parse error: " ++ str

flException (SignalException str) =
    putStrLn $ "uncaught exception: " ++ str


repl :: ReplState -> IO ()
repl state =
    do (b, state') <- (runStateT replM state
                       `catchIOError` finallyIOException state)
                      `catchFlException` finallyFlException state
       if b then return () else repl state'


batch :: String -> ReplState -> IO ()
batch ln state =
    do _ <- (runStateT (promptM ln >> return True) state
             `catchIOError` finallyIOException state)
            `catchFlException` finallyFlException state
       return ()