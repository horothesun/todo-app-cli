module LibSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property
import Lib
import Test.QuickCheck
import Test.QuickCheck.Gen

-- simpleGen :: Gen String
-- simpleGen = ...

positiveNumberGen :: Gen Int
positiveNumberGen = fmap abs arbitrary
--                  baseGen.map(i => return abs(i))
--                  baseGen.map(abs)

spec :: Spec
spec = describe "TodoApp tests" $ do

     it "title is not valid if empty" $
         isTitleValid "" `shouldBe` InvalidTitle

     modifyMaxSuccess (const 50000) $ prop "title is valid when not empty" $
         \ t -> not (null t) ==> isTitleValid t == ValidTitle t

     -- prop "title is not valid when it only contains blanks" $
        -- \ t -> isTitleValid t == InvalidTitle
