module NewTodoSpec where

import Control.Concurrent (forkIO)
import Control.Lens ( (^.), set ) 
import Data.Aeson (encode, decode, toJSON, FromJSON, ToJSON)
import Data.ByteString.Lazy.UTF8 (toString, ByteString)
import Data.Aeson (encode, decode, toJSON)
import Happstack.Server (simpleHTTP, nullConf,
    defaultBodyPolicy, toResponse, ok, resp,
    Method(GET, POST), ServerPart, Response)
import Network.HTTP.Types.Status (ok200, created201, status400)
import Network.Wreq (defaults, header, get, getWith, post, postWith, checkResponse, responseBody, responseStatus)
import Test.Hspec ( beforeAll, describe, it, shouldBe, Spec ) 
import Text.Read (readMaybe)

import NewTodo 

main :: IO ()
main = simpleHTTP nullConf $ handlers

spec :: Spec
spec = beforeAll (setUp) $ do
  describe "NewTodo" $ do
     it "should POST todo item" $ do
        response <- post "http://localhost:8000/todos" (toJSON todoItem)
        response ^. responseStatus `shouldBe` created201
        isInt (toString(response ^. responseBody)) `shouldBe` True
     it "should not POST invalid todo item" $ do
        response <- postWith (set checkResponse (Just $ \_ _ -> return ()) defaults) "http://localhost:8000/todos" (toJSON invalidTodoItem)
        response ^. responseStatus `shouldBe` status400
        toString(response ^. responseBody) `shouldBe` "{\"item\":{\"input_text\":\" \"},\"errors\":[\"InvalidDescriptionError\"]}"
 
  where
    todoItem = InputTodoItem { input_text = "example" }
    invalidTodoItem = InputTodoItem { input_text = " " }
    isInt string = (readMaybe string :: Maybe Int) /= Nothing
    setUp = do forkIO main
               return () 
