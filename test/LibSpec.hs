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

positiveNumberGen :: Gen Int
positiveNumberGen = fmap abs arbitrary

--                  baseGen.map(i => return abs(i))
--                  baseGen.map(abs)

spec :: Spec
spec = describe "TodoApp tests" $ do
  modifyMaxSuccess (const 10000) $
    prop "title is not valid if empty or blank" $
      \s -> all isSpace s ==> isTitleValid s == InvalidTitle

  modifyMaxSuccess (const 10000) $
    prop "title is valid whenver it's not empty or blank" $
      \t -> (not . all isSpace) t ==> isTitleValid t == ValidTitle t
