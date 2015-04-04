{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Stage.IndentLexer where

import Control.Applicative
import Control.Monad.State
import Data.Char (isPunctuation, isSpace, isDigit)
import Data.Maybe (isJust)
import Data.Token (Srcloc(..), Token(..))
import qualified Data.Token as Token
import qualified Data.List as List
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
  = IndentLexerState { idnFilename :: String }

newtype IndentLexer a
  = IndentLexer { unIndentLexer :: State IndentLexerState a }
  deriving (Applicative, Functor, Monad, MonadState IndentLexerState)

-- | 'section' @idns idn@ is the 'List' of 'BeginSection' and
-- 'EndSection' 'Token's issued according the indentation stack @idns@
-- and current indentation @idn@.
section :: String -> [Int] -> Int -> [Token]
section _ [] _ = error "section: idns is empty"
section c (idn1:idns) idn2 =
  case compare idn1 idn2 of
    EQ -> []
    LT -> [beginSection]
    GT -> endSection:section c (dropWhile (> idn1) idns) idn2

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

tokenize :: String -> Int -> [Int] -> String -> [Token]
tokenize filename line idns ln =
  blockifyConds $ Lexer.lexTokens filename line ln

-- 'reduce' @idns ln@ is the 'List' containing the 'Literal' 'Token'
-- holding @ln@ preceeded by the appropriate section 'Token's as
-- issued by 'section' according to the indentation stack @idns@.
reduce :: [Int] -> Int -> String -> IndentLexer [Token]
reduce idns n ln =
  do filename <- idnFilename <$> get
     return $ section "|" idns idn ++ tokenize filename n (push idn idns) (trim ln)
  where
    idn = indentation ln

classify :: [Int] -> [(Int, String)] -> IndentLexer [Token]
classify idns [] =
  return $ replicate (length idns - 1) endSection
classify idns ((n, ln):lns)
  | isEmptyLine ln = classify idns lns
classify idns [(n, ln)] =
  (++) <$> reduce idns n ln <*> classify (push (indentation ln) idns) []
classify idns ((n1, ln1):(n2, ln2):lns)
  | isEmptyLine ln2 =
    do ln1' <- reduce idns n1 ln1
       lns' <- classify (push idn1 idns) lns
       case lns' of
         [] -> return ln1'
         [endSection] -> return $ ln1' ++ [endSection]
         endSection:lns'' -> return $ ln1' ++ [endSection] ++ lns''
  | otherwise =
    (++) <$> reduce idns n1 ln1 <*> classify (push idn1 idns) ((n2, ln2):lns)
  where
    idn1 = indentation ln1
    idn2 = indentation ln2

-- | 'indentLex' @filename@ @str@ lexes the input @str@ into a list of 'Token's,
-- where @filename@ is the input filename, which is used for error messages.
indentLex :: String -> String -> [Token]
indentLex filename str =
  runMonad (IndentLexerState filename) (classify [0] $ zip [1..] $ lines str)
  where
    runMonad s m =
      fst $ runState (unIndentLexer m) s
