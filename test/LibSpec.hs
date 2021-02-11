module LibSpec where

import Data.Char
import Data.Maybe (isJust, isNothing)
import Lib
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property

notEmptyStringGen :: Gen String
notEmptyStringGen = suchThat arbitrary (not . (any isSpace))

emptyOrBlankGen :: Gen String
-- emptyOrBlankGen =  suchThat arbitrary (all isSpace)
emptyOrBlankGen = listOf spaceCharGen
  where
    spaceCharGen :: Gen Char
    spaceCharGen = suchThat arbitrary isSpace

spec :: Spec
spec = describe "TodoApp tests" $ do
  describe "checkTodo" $ do
    -- prop "check that the new implementation is consistent with the legacy one" $
    --  \s a -> (checkTodo (s :: String) (a :: Int)) == (checkTodo2 s a)

    prop "must return invalid title IF title is empty or blank" $
      forAll emptyOrBlankGen $ \s ->
        forAll arbitrary $ \a ->
          checkTodo (s :: String) (a :: Int) == TitleEmpty

    prop "must return invalid title ONLY IF title is empty or blank" $
      \s a -> (not . all isSpace) s ==> checkTodo s (a :: Int) /= TitleEmpty

  describe "trim" $ do
    prop "must be idempotent" $
      \s -> (trim . trim) s == trim s

    prop "must return empty string if the original string is empty or blank" $
      forAll emptyOrBlankGen $ \s ->
        trim s == ""

    prop "must remove empty strings at the end and at the beginning" $
      forAll emptyOrBlankGen $ \s1 ->
        forAll emptyOrBlankGen $ \s2 ->
          forAll notEmptyStringGen $ \s3 ->
            (not . null) s1 && (not . null) s2 && (not . null) s3
              ==> trim (s1 ++ s3 ++ s2) == s3

    it "remove spaces at the beginning" $
      (trim "  he ll  o ") `shouldBe` "he ll  o"
