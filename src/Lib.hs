module Lib where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

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

trim :: String -> String
trim = trimRight . trimLeft
  where
    -- trimRight = reverse . trimLeft . reverse
    trimRight = dropWhileEnd isSpace
    trimLeft = dropWhile isSpace

-- OLD

data Title = InvalidTitle | ValidTitle String deriving (Eq, Show)

isTitleValid :: String -> Title
isTitleValid "" = InvalidTitle
isTitleValid t = ValidTitle t
