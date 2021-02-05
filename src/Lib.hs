module Lib where

import Control.Monad (mfilter)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Text (strip)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Title = InvalidTitle | ValidTitle String deriving (Eq, Show)

isTitleValid :: String -> Title
isTitleValid "" = InvalidTitle
isTitleValid t = ValidTitle t

nonEmptyTitle "" = Nothing
nonEmptyTitle s = Just s

title = title2

title2 = nonEmptyTitle . trim

title3 = nonEmptyTitle . strip

trim = dropWhileEnd isSpace . dropWhile isSpace

-- Approach 3

f' s = mfilter (all isSpace) (Just s)
