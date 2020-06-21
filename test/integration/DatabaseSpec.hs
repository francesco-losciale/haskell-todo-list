module DatabaseSpec where

import Database
import Test.Hspec


spec :: Spec
spec = beforeAll (setUp) $ do
  describe "Database" $ do
    it "should persist and read todo list" $ do
       deleteAllTodos
       rows <- writeAllTodos [todoActive, todoComplete]
       todoList <- extractAllTodos
       todoList `shouldBe` expectedTodoList
  where
    todoActive = ("todo marked as active", "Active")
    todoComplete = ("todo marked as complete", "Complete")
    expectedTodoList = [todoActive, todoComplete]
    setUp = do
              deleteAllTodos
              return ()
