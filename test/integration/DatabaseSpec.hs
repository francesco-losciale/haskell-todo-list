module DatabaseSpec where

import Database
import Test.Hspec


spec :: Spec
spec = do
  describe "Database" $ do
    it "should connect to the database and query the todo table" $ do
       result <- (testQuery "select 1")
       result `shouldBe` (1 :: Int)