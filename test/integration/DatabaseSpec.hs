module DatabaseSpec where

import Todo.TodoValidation

import Database
import Test.Hspec


spec :: Spec
spec = beforeAll (setUp) $ do
  describe "Postgres Database" $ do
    it "should persist a todo in list" $ do
       deleteAllTodos
       id <- writeTodo todoActive
       todoList <- extractAllTodos
       todoList `shouldBe` [todoActive]
  where
    todoActive = Todo 1 "todo marked as active" Active
    todoComplete = Todo 2 "todo marked as complete" Complete
    expectedTodoList = [todoActive, todoComplete]
    setUp = do
              deleteAllTodos
              return ()
