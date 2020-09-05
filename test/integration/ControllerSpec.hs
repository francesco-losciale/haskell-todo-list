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
import Network.Wreq (defaults, header, get, getWith, post, responseBody, responseStatus)
import Test.Hspec 

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
    it "should POST todo item" $ do
        response <- post "http://localhost:8000/todos" (toJSON todoItem)
        response ^. responseStatus `shouldBe` created201
        toString(response ^. responseBody) `shouldBe` "[{\"state\":\"Active\",\"description\":\"example\"}]"
    it "should not POST invalid todo item" $ do
        response <- post "http://localhost:8000/todos" (toJSON invalidTodoItem)
        response ^. responseStatus `shouldBe` status400
        toString(response ^. responseBody) `shouldBe` "[\"InvalidDescriptionError\",\"InvalidStatusError\"]"
    it "should GET todo items" $ do
        let opts = defaults & header "Content-Type" .~ ["application/json"]
        response <- getWith opts "http://localhost:8000/todos"
        response ^. responseStatus `shouldBe` ok200
        (fromJust $ decode (response ^. responseBody)) `shouldBe` [todoItem]

  where
    todoItem = (Todo "example" Active)
    invalidTodoItem = (Todo "" Complete)
    setUp = do deleteAllTodos
               forkIO main
               return () 