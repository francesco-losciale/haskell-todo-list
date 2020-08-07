module TodoSpec where

import Test.Hspec

import Todo

spec :: Spec
spec = do
  describe "Todo List" $ do
    it "should create a todo item" $ do
       (Todo "something" Active) `shouldBe` (Todo "something" Active)
    it "should validation of empty description fail" $ do
       (validate (Todo "" Active)) `shouldBe` (Left InvalidDescriptionError)
    it "should create todo to list" $ do
       (createList todoActive) `shouldBe` [todoActive]
    it "should add todo to list" $ do
       (collect (createList todoActive) todoActive) `shouldBe` [todoActive, todoActive]
    it "should not add invalid todo to list" $ do
       (collect (createList todoInvalid) todoInvalid) `shouldBe` []
    it "given a todo then mark it as complete" $ do
       (Todo "something" Complete) `shouldBe` (complete (Todo "something" Active))
    it "given a todo list then mark a specific one as complete" $ do
       (apply complete todoActive todoList) `shouldBe` expectedUpdatedTodoList
    it "given a todo list then remove all the completed ones" $ do
       (removeCompleted todoList) `shouldBe` expectedPurgedTodoList
    where
        todoInvalid = (Todo " " Active)
        todoActive = (Todo "todo marked as active" Active)
        todoComplete = (Todo "todo marked as complete" Complete)
        todoList = [todoActive, todoComplete]
        expectedUpdatedTodoList = [(Todo "todo marked as active" Complete), (Todo "todo marked as complete" Complete)]
        expectedPurgedTodoList = [todoActive]
