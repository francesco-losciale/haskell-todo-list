module TodoSpec where

import Todo
import Test.Hspec

spec :: Spec
spec = do
  describe "Todo List" $ do
    it "should create a todo item" $ do
       (Todo "something" Active) `shouldBe` (Todo "something" Active)
    it "should validation of empty description fail" $ do
       (validate (Todo "" Active)) `shouldBe` (Left InvalidDescriptionError)
    it "should create todo to list" $ do
       [item] `shouldBe` (createList item)
    it "should add todo to list" $ do
       [item, item] `shouldBe` (collect (createList item) item)
    it "should not add invalid todo to list" $ do
       [] `shouldBe` (collect (createList invalidItem) invalidItem)
    it "given a todo then mark it as complete" $ do
       (Todo "something" Complete) `shouldBe` (complete (Todo "something" Active))
    it "given a todo list then mark a specific one as complete" $ do
       expectedList `shouldBe` (apply complete item list)
    it "given a todo list then remove all the completed ones" $ do
       expectedListWithoutComplete `shouldBe` (removeCompleted listWithComplete)
    where
        item = (Todo "something" Active)
        invalidItem = (Todo " " Active)
        list = [(Todo "something1" Active), item, (Todo "something3" Active)]
        expectedList = [(Todo "something1" Active), (Todo "something" Complete), (Todo "something3" Active)]
        listWithComplete = [(Todo "something1" Active), (Todo "something2" Active), (Todo "something3" Complete)]
        expectedListWithoutComplete = [(Todo "something1" Active), (Todo "something2" Active)]
