{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
module ControllerSpec where

import Control.Concurrent (forkIO)
import Control.Lens 
import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, decode, toJSON)
import Data.ByteString.Lazy.UTF8 (toString, ByteString)
import Data.Maybe (fromJust)
import Happstack.Server (simpleHTTP, nullConf,
    defaultBodyPolicy, toResponse, ok, resp,
    Method(GET, POST), ServerPart, Response)
import Network.HTTP.Types.Status (ok200, created201, status400)
import Network.Wreq (defaults, header, get, getWith, post, postWith, checkResponse, responseBody, responseStatus)
import Test.Hspec ( beforeAll, describe, it, shouldBe, Spec ) 

import Controller
import Database
import Todo.TodoValidation

main :: IO ()
main = simpleHTTP nullConf $ handlers

spec :: Spec
spec = beforeAll (setUp) $ do
  describe "Controller" $ do
    it "should be healthy" $ do
        response <- get "http://localhost:8000/health"
        response ^. responseStatus `shouldBe` ok200
        toString(response ^. responseBody) `shouldBe` "success"
    it "should not POST invalid todo item" $ do
        response <- postWith (set checkResponse (Just $ \_ _ -> return ()) defaults) "http://localhost:8000/todos" (toJSON invalidTodoItem)
        response ^. responseStatus `shouldBe` status400
        toString(response ^. responseBody) `shouldBe` "{\"item\":{\"state\":\"Complete\",\"description\":\"\"},\"errors\":[\"InvalidDescriptionError\",\"InvalidStatusError\"]}"
    it "should POST todo item" $ do
        response <- post "http://localhost:8000/todos" (toJSON todoItem)
        response ^. responseStatus `shouldBe` created201
        toString(response ^. responseBody) `shouldBe` "{\"list\":[{\"state\":\"Active\",\"description\":\"example\"}],\"newTodoId\":123}"
    it "should GET todo items" $ do
        let opts = defaults & header "Content-Type" .~ ["application/json"]
        response <- getWith opts "http://localhost:8000/todos"
        response ^. responseStatus `shouldBe` ok200
        (fromJust $ decode (response ^. responseBody)) `shouldBe` [todoItem]

  where
    todoItem = Todo { description ="example", state = Active }
    invalidTodoItem = Todo { description ="", state = Complete }
    setUp = do deleteAllTodos
               forkIO main
               return () 