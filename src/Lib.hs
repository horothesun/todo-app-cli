module Lib where

import Control.Monad (mfilter)
import Data.Char (isSpace)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Title = InvalidTitle | ValidTitle String deriving (Eq, Show)

isTitleValid :: String -> Title
isTitleValid "" = InvalidTitle
-- isTitleValid t | all isSpace t = InvalidTitle
isTitleValid t = ValidTitle t

-- Approach 2
f "" = Nothing
f s = Just s

isTitleValid2 = f . trim

trim :: String -> String
trim s = undefined

-- Approach 3

f' s = mfilter (all isSpace) (Just s)
