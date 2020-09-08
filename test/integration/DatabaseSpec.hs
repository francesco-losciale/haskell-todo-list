module DatabaseSpec where

import Todo.TodoValidation 
import Database
import Test.Hspec


spec :: Spec
spec = beforeAll (setUp) $ do
  describe "Postgres Database" $ do
    it "should persist a todo in list" $ do
       deleteAllTodos
       id <- writeTodo $ wantToWrite todoActive
       todoList <- extractList
       [ description $ toTodoItem x | x <- todoList] `shouldBe` [description $ todoActive]
  where
    todoActive = Todo {  description = "todo marked as active", state = Active }
    setUp = do
              deleteAllTodos
              return ()
