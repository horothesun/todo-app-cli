module LibSpec where

import Data.Char
import Data.Either
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
    modifyMaxSuccess (const 5000) $
      prop "check that the new implementation is consistent with the legacy one (this test is just an example)" $
        \s a b -> checkTodo (s :: String) a (b :: Int) == checkTodo' s a b

    prop "must return invalid title IF title is empty or blank" $
      forAll emptyOrBlankGen $ \s ->
        forAll arbitrary $ \a ->
          forAll arbitrary $ \b ->
            checkTodo (s :: String) a (b :: Int) == Left TitleEmpty

    prop "must return invalid title ONLY IF title is empty or blank" $
      \s a b -> (not . all isSpace) s ==> checkTodo s a (b :: Int) /= Left TitleEmpty

    prop "never return PastDueDate when no input due date is provided" $
      \s a -> checkTodo s Nothing (a :: Int) /= Left PastDueDate

    prop "never return PastDueDate when no input due date is provided" $
      \s a b -> (not . all isSpace) s && a < b ==> checkTodo s (Just a) (b :: Int) == Left PastDueDate

    prop "always successful when title non empty/blank and due date not present (redundant test)" $
      \s a -> (not . all isSpace) s ==> isRight $ checkTodo s Nothing (a :: Int)

    prop "always successful when title non empty/blank and due date is present and in the future(redundant test)" $
      \s a b -> (not . all isSpace) s && a > b ==> isRight $ checkTodo s (Just a) (b :: Int)

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

    prop "trim right should behave like trim left on the reversed string" $
      \s -> trimRight s == (reverse . trimLeft . reverse) s

    it "for example, must remove spaces at the beginning and at the end but not in the middle" $
      trim "  he ll  o " `shouldBe` "he ll  o"
