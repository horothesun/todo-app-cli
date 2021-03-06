module Lib
  ( TodoValidationError (..),
    checkTodo,
    checkTodo',
    trim,
    trimRight,
    trimLeft,
    Todo (..),
  )
where

import Control.Monad
import Data.Char (isSpace)
import Data.Either.Combinators (mapLeft, maybeToRight)
import Data.List (dropWhileEnd)

data Todo s a = Todo
  { description :: s,
    dueDate :: Maybe a,
    creationDate :: a
  }
  deriving (Eq, Show)

data TodoValidationError = TitleEmpty | PastDueDate deriving (Eq, Show)

checkTodo :: Ord p => String -> Maybe p -> p -> Either TodoValidationError (Todo String p)
checkTodo = checkTodo1

checkTodo' :: Ord p => String -> Maybe p -> p -> Either TodoValidationError (Todo String p)
checkTodo' = checkTodo2

-- checkTodo :: (Eq a, IsString a) => a -> p -> TodoResult a p
checkTodo1 :: Ord p => String -> Maybe p -> p -> Either TodoValidationError (Todo String p)
checkTodo1 s _ _ | (null . trim) s = Left TitleEmpty
checkTodo1 _ (Just a) b | a < b = Left PastDueDate
checkTodo1 s a b = Right $ Todo s a b

checkTodo2 :: Ord p => String -> Maybe p -> p -> Either TodoValidationError (Todo String p)
checkTodo2 s a b = do
  t <- mapLeft (const TitleEmpty) (checkTitle s)
  d <- mapLeft (const PastDueDate) (checkDueDate b a)
  return $ Todo t d b

checkTitle :: String -> Either () String
checkTitle = maybeToRight () . mfilter (not . null . trim) . Just

checkDueDate :: Ord p => p -> Maybe p -> Either () (Maybe p)
checkDueDate n = traverse f
  where
    f d
      | d < n = Left ()
      | otherwise = Right d

trim :: String -> String
trim = trimRight . trimLeft

trimRight :: String -> String
trimRight = dropWhileEnd isSpace

trimLeft :: String -> String
trimLeft = dropWhile isSpace
