module Lib where

import Control.Monad
import Data.Char (isSpace)
import Data.Either.Combinators
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

data TodoErrors = TitleError | PastDueDateError deriving (Eq, Show)

checkTodo :: Ord p => String -> Maybe p -> p -> TodoResult String p
checkTodo = checkTodoConvert checkTodo1

checkTodo' :: Ord p => String -> Maybe p -> p -> TodoResult String p
checkTodo' = checkTodoConvert checkTodo2

-- checkTodo :: (Eq a, IsString a) => a -> p -> TodoResult a p
checkTodo1 :: Ord p => String -> Maybe p -> p -> Either TodoErrors (Todo String p)
checkTodo1 s _ _ | (null . trim) s = Left TitleError
checkTodo1 _ (Just a) b | a < b = Left PastDueDateError
checkTodo1 s a b = Right $ Todo s a b

checkTodo2 :: Ord p => String -> Maybe p -> p -> Either TodoErrors (Todo String p)
checkTodo2 s a b = do
  t <- maybeToRight TitleError (checkTitle s)
  d <- mapLeft (const PastDueDateError) (checkDueDate b a)
  return $ Todo t d b

checkTitle :: String -> Maybe String
checkTitle = mfilter (not . null . trim) . Just

checkDueDate :: Ord p => p -> Maybe p -> Either () (Maybe p)
checkDueDate _ Nothing = Right Nothing
checkDueDate n (Just d)
  | d < n = Left ()
  | otherwise = Right (Just d)

trim :: String -> String
trim = trimRight . trimLeft

trimRight :: String -> String
trimRight = dropWhileEnd isSpace

trimLeft :: String -> String
trimLeft = dropWhile isSpace

checkTodoConvert :: (String -> Maybe a -> a -> Either TodoErrors (Todo s a)) -> String -> Maybe a -> a -> TodoResult s a
checkTodoConvert f s a b = convert $ f s a b
  where
    convert :: Either TodoErrors (Todo s a) -> TodoResult s a
    convert (Left TitleError) = TitleEmpty
    convert (Left PastDueDateError) = PastDueDate
    convert (Right t) = ValidTodo t
