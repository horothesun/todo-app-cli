module LibSpec where

import Data.Char
import Lib
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property

-- import Test.QuickCheck.Gen

-- simpleGen :: Gen String
-- simpleGen = ...

spaceCharGen :: Gen Char
spaceCharGen = suchThat arbitrary isSpace

emptyOrBlankStringGen :: Gen String
emptyOrBlankStringGen = listOf spaceCharGen

positiveNumberGen :: Gen Int
positiveNumberGen = fmap abs arbitrary

--                  baseGen.map(i => return abs(i))
--                  baseGen.map(abs)

spec :: Spec
spec = describe "TodoApp tests" $ do
  modifyMaxSuccess (const 5000) $
    prop "title is not valid if empty or blank" $
      forAll emptyOrBlankStringGen $
        \t -> isTitleValid t == InvalidTitle

  modifyMaxSuccess (const 5000) $
    prop "title is valid whenver it's not empty or blank" $
      \t -> (not . all isSpace) t ==> isTitleValid t /= InvalidTitle
