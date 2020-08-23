
{-# LANGUAGE OverloadedStrings #-}
module JsonSerializerSpec where

import Todo.TodoValidation

import Test.Hspec
import Data.Aeson as AS
import Data.ByteString.Lazy.UTF8 as BS
import Data.Maybe as MB


spec :: Spec
spec = 
  describe "Json serialization" $ do
    it "should decode TodoItem from json" $ do
        let todoItem = MB.fromJust $ AS.decode $ BS.fromString jsonString
        todoItem `shouldBe` todoItemExample
    it "should encode TodoItem to json " $ do
        (AS.encode todoItemExample) `shouldBe` BS.fromString jsonString
  where
    jsonString = "{\"state\":\"Active\",\"description\":\"test\"}"
    todoItemExample = Todo {description = "test", state = Active}
