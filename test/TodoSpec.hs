{-# LANGUAGE OverloadedStrings #-}
module TodoSpec where

import Control.Concurrent (forkIO)
import Control.Lens ( (^.), set, (&), (.~) ) 
import Data.Aeson (encode, decode, toJSON, FromJSON, ToJSON)
import Data.ByteString.Lazy.UTF8 (toString, ByteString)
import Data.Aeson (encode, decode, toJSON)
import Data.Maybe (fromJust)
import Happstack.Server (simpleHTTP, nullConf,
    defaultBodyPolicy, toResponse, ok, resp,
    Method(GET, POST), ServerPart, Response)
import Network.HTTP.Types.Status (ok200, created201, status400)
import Network.Wreq (defaults, header, get, getWith, post, customPayloadMethodWith, postWith, checkResponse, responseBody, responseStatus)
import Test.Hspec ( beforeAll, describe, it, shouldBe, Spec ) 
import Text.Read (readMaybe)

import Todo
import Data.Aeson.Types (fromJSON)

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
