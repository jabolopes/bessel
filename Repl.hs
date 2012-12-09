{-# LANGUAGE BangPatterns #-}
module Repl where

import Prelude hiding (lex, catch)

import Control.Monad.State
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map ((!), insert)
import System.Console.Readline

--import System.IO.Error (catchIOError)
import System.IO.Error (catch)

import Data.Exception
import Data.Type
import Data.Stx
import qualified DynamicLibrary (env, symbols, symbolTs)
import Interpreter
import Lexer
import Loader
import Parser
import Printer.PrettyStx (prettyPrint)
import Renamer
import Typechecker


data ReplState = ReplState RenamerState ExprEnv (Map String Type) [Stx String]
type ReplM a = StateT ReplState IO a


catchIOError :: IO a -> (IOError -> IO a) -> IO a
catchIOError = catch


doPutLine = False
doPutTokens = False
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
        do putStrLn "Environment"
           putStrLn $ show env
           putStrLn ""
    | otherwise = return ()


preludeName :: String
preludeName = "Prelude"


renamerEither :: Either String a -> a
renamerEither fn = either throwRenamerException id fn


typecheckerEither :: Either String a -> a
typecheckerEither fn = either throwTypecheckerException id fn


-- info: calls to 'liftIO' are necessary to force exceptions to bleed through
-- edit: the bang pattern forces evaluation of the typechecker
importFile :: Map String Type -> [Stx String] -> String -> IO ReplState
importFile symbols prelude filename =
    do (deps, mods) <- preloadWithPrelude preludeName prelude filename
       (stxs, renamerState) <- liftIO $ return $ renamerEither $ rename DynamicLibrary.symbols deps mods
       let (_, exprEnv) = interpret DynamicLibrary.env stxs
       !symbols' <- liftIO $ return $ typecheckerEither $ typecheckStxs symbols stxs
       return $ ReplState (mkInteractiveFrame [preludeName, filename] renamerState) exprEnv symbols' prelude


-- info: calls to 'liftIO' are necessary to force exceptions to bleed through
-- edit: the bang pattern forces evaluation of the typechecker
importPrelude :: IO ReplState
importPrelude =
    do (deps, mods) <- preload preludeName
       (stxs, renamerState) <- liftIO $ return $ renamerEither $ rename DynamicLibrary.symbols deps mods
       let (_, exprEnv) = interpret DynamicLibrary.env stxs
       !symbols <- liftIO $ return $ typecheckerEither $ typecheckStxs DynamicLibrary.symbolTs stxs
       return $ ReplState (mkInteractiveFrame [preludeName] renamerState) exprEnv symbols (mods Map.! preludeName)


runSnippetM :: String -> ReplM ()
runSnippetM ln =
    do ReplState renamerState exprEnv symbols prelude <- get
       let tokens = lex ln
           stx = parseDefnOrExpr tokens
           (stx', renamerState') = renamerEither $ renameIncremental renamerState stx
       liftIO $ putRenamedStx stx'
       case typecheckIncremental symbols stx' of
         Left str -> throwTypecheckerException str
         Right (t, symbols') -> do let (expr, exprEnv') = interpret exprEnv [stx']
                                   liftIO $ do
                                     putExprT expr t
                                     -- putStrLn ""
                                     -- putStrLn $ show symbols'
                                     -- putStrLn ""
                                     putEnvironment exprEnv'
                                   put $ ReplState renamerState' exprEnv' symbols' prelude

promptM :: String -> ReplM Bool
promptM ln =
    do liftIO $ do
         addHistory ln
         putLine ln
       process ln
    where processM modName =
              do ReplState _ _ symbols prelude <- get
                 liftIO (importFile symbols prelude modName) >>= put
                 return False

          process :: String -> ReplM Bool
          process (':':line)
                  | "load \"" `isPrefixOf` line = processM $ init $ tail $ dropWhile (/= '"') line
                  | "load " `isPrefixOf` line = processM $ tail $ dropWhile (/= ' ') line
                  | "l " `isPrefixOf` line = processM $ tail $ dropWhile (/= ' ') line
                  | otherwise = do liftIO $ putStrLn $ "command error: " ++ show ln ++ " is not a valid command"
                                   replM

          process ln =
              do runSnippetM ln
                 return False


replM :: ReplM Bool
replM =
    do mprompt <- liftIO $ readline "fl$ "
       case mprompt of 
         Nothing -> return True
         Just ln | ln == ":quit" -> return True
                 | all isSpace ln -> replM
                 | otherwise -> promptM ln


repl :: ReplState -> IO ()
repl state =
    do (b, state') <- (runStateT replM state
                       `catchIOError` finallyIOException state)
                      `catchFlException` finallyFlException state
       if b then return () else repl state'
    where finallyIOException state e =
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