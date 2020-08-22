module Todo.TodoValidationSpec where

import Test.Hspec
import Data.List.NonEmpty

import Todo.TodoValidation

spec :: Spec
spec = do
  describe "TodoValidation List" $ do
    it "should create a todo item" $ do
       (Todo "something" Active) `shouldBe` (Todo "something" Active)
    it "should fail validation of an item with empty description" $ do
       validateDescription todoEmptyDescription `shouldBe` Just InvalidDescriptionError
    it "should fail validation of an item with completed status" $ do
       validateState todoComplete `shouldBe` Just InvalidStatusError
    it "should fail validation with multiple errors" $ do
       addValidatedTodo defaultValidations (Todo " " Complete)  [] `shouldBe` (Left $ InvalidDescriptionError :| [InvalidStatusError])
    it "should add todo to list when no errors" $ do
       addValidatedTodo defaultValidations todoActive  [] `shouldBe` (Right $ [todoActive])
    it "should not add todo to list when errors" $ do
       addValidatedTodo defaultValidations todoComplete  [] `shouldBe` (Left $ InvalidStatusError :| [])
    it "given a todo then mark it as complete" $ do
       (Todo "something" Complete) `shouldBe` (complete (Todo "something" Active))
    it "given a todo list then mark a specific one as complete" $ do
       (apply complete todoActive todoList) `shouldBe` expectedUpdatedTodoList
    it "given a todo list then remove all the completed ones" $ do
       (removeCompleted todoList) `shouldBe` expectedPurgedTodoList
    where
        todoEmptyDescription = (Todo " " Active)
        todoActive = (Todo "todo marked as active" Active)
        todoComplete = (Todo "todo marked as complete" Complete)
        todoList = [todoActive, todoComplete]
        expectedUpdatedTodoList = [(Todo "todo marked as active" Complete), (Todo "todo marked as complete" Complete)]
        expectedPurgedTodoList = [todoActive]
