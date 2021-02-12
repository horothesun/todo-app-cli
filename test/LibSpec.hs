module LibSpec where

import Data.Char
import Lib
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

notEmptyStringGen :: Gen String
notEmptyStringGen = suchThat arbitrary (not . any isSpace)

emptyOrBlankGen :: Gen String
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

    prop "must always return empty string if the original string is empty or blank" $
      forAll emptyOrBlankGen $ \s ->
        trim s == ""

    prop "must behave like the identify on the subset of strings not containing space characters" $
      forAll notEmptyStringGen $ \s -> trim s == s

    prop "must remove empty strings at the end and at the beginning but not in the middle" $
      forAll (suchThat emptyOrBlankGen (not . null)) $ \s1 ->
        forAll (suchThat notEmptyStringGen (not . null)) $ \s2 ->
          forAll (suchThat emptyOrBlankGen (not . null)) $ \s3 ->
            forAll (suchThat notEmptyStringGen (not . null)) $ \s4 ->
              forAll (suchThat emptyOrBlankGen (not . null)) $ \s5 ->
                trim (s1 ++ s2 ++ s3 ++ s4 ++ s5) == s2 ++ s3 ++ s4

    it "for example, must remove spaces at the beginning and at the end but not in the middle" $
      trim "  he ll  o " `shouldBe` "he ll  o"
