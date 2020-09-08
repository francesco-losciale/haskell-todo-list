module NewTodoSpec where

import Control.Concurrent (forkIO)
import Control.Lens ( (^.) ) 
import Data.Aeson (encode, decode, toJSON, FromJSON, ToJSON)
import Data.ByteString.Lazy.UTF8 (toString, ByteString)
import Data.Aeson (encode, decode, toJSON)
import Happstack.Server (simpleHTTP, nullConf,
    defaultBodyPolicy, toResponse, ok, resp,
    Method(GET, POST), ServerPart, Response)
import Network.HTTP.Types.Status (ok200, created201, status400)
import Network.Wreq (defaults, header, get, getWith, post, postWith, checkResponse, responseBody, responseStatus)
import Test.Hspec ( beforeAll, describe, it, shouldBe, Spec ) 

import NewTodo 

main :: IO ()
main = simpleHTTP nullConf $ handlers

spec :: Spec
spec = beforeAll (setUp) $ do
  describe "NewTodo" $ do
     it "should POST todo item" $ do
        response <- post "http://localhost:8000/todos" (toJSON todoItem)
        response ^. responseStatus `shouldBe` created201
        toString(response ^. responseBody) `shouldBe` "{\"list\":[{\"state\":\"Active\",\"description\":\"example\"}],\"newTodoId\":123}"
 
  where
    todoItem = InputTodoItem { input_text = "example" }
    setUp = do forkIO main
               return () 
