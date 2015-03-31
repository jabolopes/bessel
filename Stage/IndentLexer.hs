module Stage.IndentLexer where

import Data.Char (isPunctuation, isSpace, isDigit)
import Data.Maybe (isJust)
import Data.Token (Srcloc(..), Token(..))
import qualified Data.Token as Token
import qualified Data.List as List
import qualified Lexer

import Debug.Trace

-- 'push' @x xs@ adds @x@ to @xs@ only if the first element in @xs@
-- is different from @x@.
--
-- > push 1 [2,3] == [1,2,3]
-- > push 1 [1,3] == [1,3]
push :: Ord a => a -> [a] -> [a]
push x xs = x:dropWhile (>= x) xs

beginSection :: Token
beginSection = TokenLEnvParen (Srcloc 0 0)

endSection :: Token
endSection = TokenREnvParen (Srcloc 0 0)

semicolon :: Int -> Token
semicolon column = TokenSemicolon $ Srcloc 0 column

-- | 'indentation' @ln@ is the number of space characters at the
-- begining of @ln@.
--
-- > indentation "\t hello" == 2
indentation :: String -> Int
indentation ln = length $ takeWhile isSpace ln

-- | 'trim' @str@ removes leading and trailing space characters from
-- @str@.
--
-- > trim "\t hello \n" == "hello"
trim :: String -> String
trim = List.dropWhileEnd isSpace . List.dropWhile isSpace

isEmptyLn :: String -> Bool
isEmptyLn = all isSpace

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
    (xs, y@TokenEquiv {}:ys) | not (null ys) && not (containsIn ys) ->
      xs ++ [y, beginSection] ++ ys ++ [endSection]
    (xs, y@TokenEquiv {}:ys) | not (null ys) && containsIn ys ->
      let (ys', tokenIn) = dropIn ys in
      xs ++ [y, beginSection] ++ ys' ++ [endSection, tokenIn]
    _ ->
      tokens
  where
    isEquiv TokenEquiv {} = True
    isEquiv _ = False

    isIn TokenIn {} = True
    isIn _ = False

    containsIn xs =
      case List.find isIn xs of
        Nothing -> False
        Just _ -> True

    dropIn xs =
      case List.break isIn xs of
        (xs, y@TokenIn {}:ys) -> (xs ++ ys, y)

tokenize :: Int -> [Int] -> String -> [Token]
tokenize n idns ln = blockifyConds $ Lexer.lexTokens "" ln

-- 'reduce' @idns ln@ is the 'List' containing the 'Literal' 'Token'
-- holding @ln@ preceeded by the appropriate section 'Token's as
-- issued by 'section' according to the indentation stack @idns@.
reduce :: [Int] -> Int -> String -> [Token]
reduce idns n ln =
  section "|" idns idn ++ tokenize n (push idn idns) (trim ln)
  where
    idn = indentation ln

sectionTBlockStarter :: [Char]
sectionTBlockStarter = "|"

blockStarterChars :: [Char]
blockStarterChars = "*\"|>"

blockStarter :: String -> Maybe (String, String)
blockStarter (c:' ':ln)
    | c `elem` blockStarterChars = Just ([c], replicate 2 ' ' ++ ln)
    | otherwise = Nothing
blockStarter ln =
    case span isDigit ln of
      ([], _) -> Nothing
      (ds, '.':' ':ln) -> Just (ds, replicate (length ds + 2) ' ' ++ ln)
      _ -> Nothing

-- | 'indentLex' @str@ is the 'List' of 'Token's of @str@.
indentLex :: String -> [Token]
indentLex str =
  classify' [0] $ zip [1..] $ lines str
  where
    classify' :: [Int] -> [(Int, String)] -> [Token]
    classify' idns [] =
      replicate (length idns - 1) endSection

    classify' idns ((n, ln):lns)
      | isEmptyLn ln = classify' idns lns
      | isJust $ blockStarter (dropWhile isSpace ln) =
        let
          pre = takeWhile isSpace ln
          Just (cs, suf) = blockStarter (dropWhile isSpace ln)
          ln' = pre ++ suf
          s1 = section sectionTBlockStarter idns (indentation ln)
          idns' = drop (length s1) idns
          s2 = section cs (push (indentation ln) idns') (indentation ln')
          lns' = classify' (push (indentation ln') idns') $ (n, ln'):lns
        in
         s1 ++ s2 ++ lns'

    classify' idns [(n, ln)] =
      reduce idns n ln ++ classify' (push (indentation ln) idns) []

    classify' idns ((n1, ln1):(n2, ln2):lns)
      | isEmptyLn ln2 =
        let
          ln1' = reduce idns n1 ln1
          lns' = classify' (push idn1 idns) lns
        in
         case lns' of
           [] -> ln1'
           [endSection] -> ln1' ++ [endSection]
           endSection:lns'' -> ln1' ++ [endSection] ++ lns''
      | otherwise =
        reduce idns n1 ln1 ++ classify' (push idn1 idns) ((n2, ln2):lns)
      where
        idn1 = indentation ln1
        idn2 = indentation ln2
