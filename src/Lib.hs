module Lib where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

someFunc :: IO ()
someFunc = do
  putStrLn "someFunc"

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
checkTodo :: String -> Maybe p -> p -> TodoResult String p
checkTodo s _ _ | (null . trim) s = TitleEmpty
checkTodo _ _ _ = PastDueDate

checkTodo2 :: String -> Maybe p -> p -> TodoResult String p
checkTodo2 = checkTodo

trim :: String -> String
trim = trimRight . trimLeft

trimRight = dropWhileEnd isSpace

trimLeft = dropWhile isSpace

-- OLD

data Title = InvalidTitle | ValidTitle String deriving (Eq, Show)

isTitleValid :: String -> Title
isTitleValid "" = InvalidTitle
isTitleValid t = ValidTitle t
