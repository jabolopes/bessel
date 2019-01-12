{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Stage.IndentLexer where

import Control.Monad.State
import Data.Char (isSpace)
import qualified Data.List as List

import Data.Name (Name)
import Data.Token (Token(..))
import qualified Lexer

-- 'push' @x xs@ adds @x@ to @xs@ only if the first element in @xs@
-- is different from @x@.
--
-- > push 1 [2,3] == [1,2,3]
-- > push 1 [1,3] == [1,3]
push :: Ord a => a -> [a] -> [a]
push x xs = x:dropWhile (>= x) xs

beginSection :: Token
beginSection = TokenLEnvParen

endSection :: Token
endSection = TokenREnvParen

-- | 'indentation' @ln@ is the number of space characters at the beginning of @ln@.
--
-- > indentation "\t hello" == 2
indentation :: String -> Int
indentation ln = length $ takeWhile isSpace ln

-- | 'trim' @str@ removes leading and trailing space characters from @str@.
--
-- > trim "\t hello \n" == "hello"
trim :: String -> String
trim = List.dropWhileEnd isSpace . List.dropWhile isSpace

isEmptyLine :: String -> Bool
isEmptyLine = all isSpace

data IndentLexerState
  = IndentLexerState { idnModname :: Name }

newtype IndentLexer a
  = IndentLexer { unIndentLexer :: State IndentLexerState a }
  deriving (Applicative, Functor, Monad, MonadState IndentLexerState)

-- | 'section' @idns idn@ is the 'List' of 'BeginSection' and
-- 'EndSection' 'Token's issued according the indentation stack @idns@
-- and current indentation @idn@.
section :: [Int] -> Int -> [Token]
section [] _ = error "section: idns is empty"
section (idn1:idns) idn2 =
  case compare idn1 idn2 of
    EQ -> []
    LT -> [beginSection]
    GT -> endSection:section (dropWhile (> idn1) idns) idn2

blockifyConds :: [Token] -> [Token]
blockifyConds tokens =
  case List.break isEquiv tokens of
    (xs, y@TokenEquiv {}:ys) | not (null ys) ->
      xs ++ [y, beginSection] ++ close 0 ys
    _ ->
      tokens
  where
    isEquiv TokenEquiv {} = True
    isEquiv _ = False

    close :: Int -> [Token] -> [Token]
    close _ [] = [endSection]
    close _ xs@(TokenIn {}:_) =
      endSection:xs
    close n (x@TokenLParen {}:xs) =
      x:close (n + 1) xs
    close n (x@TokenRParen {}:xs)
      | n > 0 = x:close (n - 1) xs
    close 0 xs@(TokenRParen{}:_) =
      endSection:xs
    close n (x:xs) =
      x:close n xs

tokenize :: String -> Int -> String -> [Token]
tokenize filename line ln =
  blockifyConds $ Lexer.lexTokens filename line ln

-- 'reduce' @idns ln@ is the 'List' containing the 'Literal' 'Token'
-- holding @ln@ preceeded by the appropriate section 'Token's as
-- issued by 'section' according to the indentation stack @idns@.
reduce :: [Int] -> Int -> String -> IndentLexer [Token]
reduce idns n ln =
  do modName <- idnModname <$> get
     return $ section idns idn ++ tokenize (show modName) n (trim ln)
  where
    idn = indentation ln

classify :: [Int] -> [(Int, String)] -> IndentLexer [Token]
classify idns [] =
  return $ replicate (length idns - 1) endSection
classify idns ((_, ln):lns)
  | isEmptyLine ln = classify idns lns
classify idns [(n, ln)] =
  (++) <$> reduce idns n ln <*> classify (push (indentation ln) idns) []
classify idns ((n1, ln1):(n2, ln2):lns)
  | isEmptyLine ln2 =
    do ln1' <- reduce idns n1 ln1
       lns' <- classify (push idn1 idns) lns
       case lns' of
         [] -> return ln1'
         [finishSection] -> return $ ln1' ++ [finishSection]
         finishSection:lns'' -> return $ ln1' ++ [finishSection] ++ lns''
  | otherwise =
    (++) <$> reduce idns n1 ln1 <*> classify (push idn1 idns) ((n2, ln2):lns)
  where
    idn1 = indentation ln1

-- | 'indentLex' @modName@ @str@ lexes the input @str@ into a list of 'Token's,
-- where @modName@ is the input module name, which is used for error messages.
indentLex :: Name -> String -> [Token]
indentLex modName str =
  runMonad (IndentLexerState modName) (classify [0] $ zip [1..] $ lines str)
  where
    runMonad s m =
      fst $ runState (unIndentLexer m) s
