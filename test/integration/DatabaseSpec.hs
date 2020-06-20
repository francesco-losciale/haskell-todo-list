module DatabaseSpec where

import Database
import Test.Hspec


spec :: Spec
spec = do
  describe "Database" $ do
    it "should connect to the database and query the todo table" $ do
       todoList <- extractAllTodos
       todoList `shouldBe` [("test", "Active"), ("test1", "Active")]

-- should close connection?
-- initialize the rows at the setup and deletes them at after all
