module LibSpec where

import Data.Char
import Data.Maybe (isJust, isNothing)
import Lib (title)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property

spaceCharGen :: Gen Char
spaceCharGen = suchThat arbitrary isSpace

emptyOrBlankStringGen :: Gen String
emptyOrBlankStringGen = listOf spaceCharGen

spec :: Spec
spec = describe "TodoApp tests" $ do
  modifyMaxSuccess (const 5000) $
    prop "title is not valid if empty or blank" $
      forAll emptyOrBlankStringGen $ \t -> isNothing $ title t

  modifyMaxSuccess (const 5000) $
    prop "title is valid whenver it's not empty or blank" $
      \t -> (not . all isSpace) t ==> isJust $ title t
