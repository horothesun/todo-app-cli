module Lib where

import Control.Monad (mfilter)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.String
import Data.Text (strip)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data TodoResult s a
  = TitleEmpty
  | PastDueDate
  | Todo
      { description :: s,
        dueDate :: Maybe a,
        creationDate :: a
      }
  deriving (Eq, Show)

-- checkTodo :: (Eq a, IsString a) => a -> p -> TodoResult a p
checkTodo :: String -> p -> TodoResult String p
checkTodo s _ | (null . trim) s = TitleEmpty
checkTodo _ _ = PastDueDate

checkTodo2 :: s -> a -> TodoResult s a
checkTodo2 = undefined

-- " 1 " -> "1"
trim :: String -> String
trim = trimRight . trimLeft

-- trimRight = reverse . trimLeft . reverse

trimLeft :: String -> String
-- trimLeft = dropWhile isSpace
trimLeft s = fst (myfold accumulate s ("", True))
  where
    accumulate c (s, b)
      | isSpace c && b = (s, b)
      | otherwise = ([c] ++ s, False)

size [] = 0
size (_ : t) = 1 + size t

myfold f [] a = a
myfold f (h : t) a = f h (myfold f t a)

trimRight = dropWhileEnd isSpace

{-}
trimLeft [] = []
trimLeft s@(h : t)
  | isSpace h = trimLeft t
  | otherwise = s
-}
-- OLD

data Title = InvalidTitle | ValidTitle String deriving (Eq, Show)

isTitleValid :: String -> Title
isTitleValid "" = InvalidTitle
isTitleValid t = ValidTitle t
