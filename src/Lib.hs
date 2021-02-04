module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Title = InvalidTitle | ValidTitle String deriving (Eq, Show)

isTitleValid :: String -> Title
isTitleValid "" = InvalidTitle
isTitleValid " " = InvalidTitle
isTitleValid t  = ValidTitle t

