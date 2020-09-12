{-# LANGUAGE OverloadedStrings #-}
module TodoSpec where

import Todo
import Server ( handlers )

import Control.Lens ( (^.), set, (&), (.~) ) 
import Test.Hspec (shouldBe, it, describe, beforeAll, Spec)
import Happstack.Server (simpleHTTP)
import Happstack.Server.Types (nullConf)
import Data.Aeson.Types (ToJSON(toJSON))
import Network.HTTP.Types.Status (created201)
import Data.Maybe (fromJust)
import Data.Aeson (decode)
import Network.Wreq (getWith, defaults, postWith, post)
import Network.Wreq.Lens (responseStatus)
import Data.ByteString.Lazy.UTF8 (toString)
import Network.Wreq.Lens (responseBody)
import Network.Wreq.Lens (checkResponse)
import Network.HTTP.Types (status400)
import Network.Wreq.Lens (header)
import Network.HTTP.Types.Status (ok200)
import Network.Wreq (customPayloadMethodWith)
import Text.Read (readMaybe)
import Control.Concurrent (forkIO)


main :: IO ()
main = simpleHTTP nullConf $ handlers

spec :: Spec
spec = beforeAll (setUp) $ do
  describe "Todo" $ do
     it "should POST todo item" $ do
        response <- post "http://localhost:8000/todos" (toJSON todoItem)
        response ^. responseStatus `shouldBe` created201
        isInt (toString(response ^. responseBody)) `shouldBe` True
     it "should not POST invalid todo item" $ do
        response <- postWith (set checkResponse (Just $ \_ _ -> return ()) defaults) "http://localhost:8000/todos" (toJSON invalidTodoItem)
        response ^. responseStatus `shouldBe` status400
        toString(response ^. responseBody) `shouldBe` "{\"item\":{\"input_text\":\" \"},\"errors\":[\"InvalidDescriptionError\"]}"
     it "should GET todo items" $ do
        post "http://localhost:8000/todos" (toJSON todoItem)
        post "http://localhost:8000/todos" (toJSON todoItem)
        let opts = defaults & header "Content-Type" .~ ["application/json"]
        response <- getWith opts "http://localhost:8000/todos"
        response ^. responseStatus `shouldBe` ok200
        let result = (fromJust $ decode (response ^. responseBody)) 
        and [ text x == "example" | x <- result ]`shouldBe` True
     it "should GET  a specific todo item" $ do
        postResponse <- post "http://localhost:8000/todos" (toJSON todoItem)
        postResponse ^. responseStatus `shouldBe` created201
        let id = toString(postResponse ^. responseBody)
        let uri = concat ["http://localhost:8000/todos/", id]
        let opts = defaults & header "Content-Type" .~ ["application/json"]
        response <- getWith opts uri
        response ^. responseStatus `shouldBe` ok200
        [TodoItem {todo_id = toInt id, text = input_text todoItem, state = Active }] `shouldBe` (fromJust $ decode (response ^. responseBody))
     it "should PATCH todo state to Complete" $ do
        postResponse <- post "http://localhost:8000/todos" (toJSON todoItem)
        postResponse ^. responseStatus `shouldBe` created201
        let id = toString(postResponse ^. responseBody)
        
        let opts = defaults & header "Content-Type" .~ ["application/json"]
        let uri = concat ["http://localhost:8000/todos/", id]
        patchResponse <- customPayloadMethodWith "PATCH" opts uri (toJSON $ UpdatedTodoItem { newState = Complete})

        let uri = concat ["http://localhost:8000/todos/", id]
        let opts = defaults & header "Content-Type" .~ ["application/json"]
        response <- getWith opts uri
        response ^. responseStatus `shouldBe` ok200 
        (fromJust $ decode (response ^. responseBody)) `shouldBe` [TodoItem {todo_id = toInt id, text = input_text todoItem, state = Complete }]
 
  where
    todoItem = InputTodoItem { input_text = "example" }
    invalidTodoItem = InputTodoItem { input_text = " " }
    isInt string = (readMaybe string :: Maybe Int) /= Nothing
    toInt string = fromJust (readMaybe string :: Maybe Int)
    setUp = do forkIO main
               return () 
