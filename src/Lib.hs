module Lib where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

someFunc :: IO ()
someFunc = do
  putStrLn "someFunc"

data Todo s a = Todo
  { description :: s,
    dueDate :: Maybe a,
    creationDate :: a
  }
  deriving (Eq, Show)

data TodoResult s a
  = TitleEmpty
  | PastDueDate
  | ValidTodo (Todo s a)
  deriving (Eq, Show)

-- checkTodo :: (Eq a, IsString a) => a -> p -> TodoResult a p
checkTodo :: Ord p => String -> Maybe p -> p -> TodoResult String p
checkTodo s _ _ | (null . trim) s = TitleEmpty
checkTodo _ (Just a) b | a < b = PastDueDate
checkTodo s a b = ValidTodo $ Todo s a b

checkTodo2 :: Ord p => String -> Maybe p -> p -> TodoResult String p
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
