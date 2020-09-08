module Todo.TodoValidationSpec where

import Test.Hspec
import Data.List.NonEmpty

import Todo.TodoValidation

spec :: Spec
spec = do
  describe "TodoValidation List" $ do
    it "should create a todo item" $ do
       Todo { todo_id = 1, description ="something", state = Active } `shouldBe` Todo { todo_id = 1, description ="something", state = Active }
    it "should fail validation of an item with empty description" $ do
       validateDescription todoEmptyDescription `shouldBe` Just InvalidDescriptionError
    it "should fail validation of an item with completed status" $ do
       validateState todoComplete `shouldBe` Just InvalidStatusError
    it "should fail validation with multiple errors" $ do
       addValidatedTodo defaultValidations Todo { todo_id = 1, description =" ", state = Complete }  [] `shouldBe` (Left $ InvalidDescriptionError :| [InvalidStatusError])
    it "should add todo to list when no errors" $ do
       addValidatedTodo defaultValidations todoActive  [] `shouldBe` (Right $ [todoActive])
    it "should not add todo to list when errors" $ do
       addValidatedTodo defaultValidations todoComplete  [] `shouldBe` (Left $ InvalidStatusError :| [])
    it "given a todo then mark it as complete" $ do
       Todo { todo_id = 1, description ="something", state = Complete } `shouldBe` (complete Todo { todo_id = 1, description ="something", state = Active })
    it "given a todo list then mark a specific one as complete" $ do
       (apply complete todoActive todoList) `shouldBe` expectedUpdatedTodoList
    it "given a todo list then remove all the completed ones" $ do
       (removeCompleted todoList) `shouldBe` expectedPurgedTodoList
    where
        todoEmptyDescription = Todo { todo_id = 1, description =" ", state = Active }
        todoActive = Todo { todo_id = 1, description ="todo marked as active", state = Active }
        todoComplete = Todo { todo_id = 1, description ="todo marked as complete", state = Complete }
        todoList = [todoActive, todoComplete]
        expectedUpdatedTodoList = [Todo { todo_id = 1, description ="todo marked as active", state = Complete }, Todo { todo_id = 1, description ="todo marked as complete", state = Complete }]
        expectedPurgedTodoList = [todoActive]
