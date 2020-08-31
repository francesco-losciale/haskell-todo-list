{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
module ControllerSpec where

import Control.Concurrent (forkIO)
import Control.Lens 
import Control.Monad.IO.Class (liftIO)
import Control.Monad (msum)
import Data.Aeson (encode, decode, toJSON)
import Data.ByteString.Lazy.UTF8 (toString, ByteString)
import Data.Maybe (fromJust)
import Happstack.Server (askRq, dir, method, simpleHTTP, nullConf, decodeBody, 
    defaultBodyPolicy, takeRequestBody, toResponse, ok, unBody, 
    Method(GET,POST), ServerPart, Response)
import Network.Wreq (get, post, responseBody, responseStatus)
import Network.HTTP.Types.Status (ok200)
import Test.Hspec 

import Todo.TodoValidation

-- https://stackoverflow.com/questions/8865793/how-to-create-json-rest-api-with-happstack-json-body
getBody :: ServerPart ByteString
getBody = do
    req  <- askRq 
    body <- liftIO $ takeRequestBody req 
    case body of 
        Just rqbody -> return . unBody $ rqbody 
        Nothing     -> return "" 

postTodoItemHandler :: ServerPart Response
postTodoItemHandler =
      do body <- getBody
         let todo = fromJust $ decode body :: TodoItem
         ok $ toResponse $ encode todo 

handlers :: ServerPart Response
handlers = do 
            decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
            msum [
                dir "health" $ do method GET 
                                  ok (toResponse "hello"),
                postTodoItemHandler                                  
             ]

main :: IO ()
main = simpleHTTP nullConf $ handlers

spec :: Spec
spec = beforeAll (setUp) $ do
  describe "Controller" $ do
    it "should be healthy" $ do
        response <- get "http://localhost:8000/health"
        toString(response ^. responseBody) `shouldBe` "hello"
        response ^. responseStatus `shouldBe` ok200
    it "should POST todo item" $ do
        response <- post "http://localhost:8000/todos" (toJSON todoItem)
        toString(response ^. responseBody) `shouldBe` "{\"state\":\"Active\",\"description\":\"example\"}"      

  where
    todoItem = (Todo "example" Active)
    setUp = do forkIO main
               return () 