{-# LANGUAGE ExtendedDefaultRules #-}
module ServerSpec where

import Todo.TodoValidation 

import Test.Hspec

import Happstack.Lite as Server
import Network.Wreq as Client
import Data.ByteString.Lazy.UTF8 as ByteString
import Control.Lens
import Control.Concurrent

anApp :: ServerPart Server.Response
anApp = msum
  [
    Server.dir "health" $ return (toResponse "hello"),
    Server.dir "get-empty-json" $ return (toResponse "{}"),
    Server.dir "get-unit" $ return (toResponse "{\"x\":1,\"y\":2}")
  ]

spec :: Spec
spec = beforeAll (setUp) $ do
  describe "Web Server" $ do
    it "should contact server" $ do
        response <- Client.get "http://localhost:8000/health"
        ByteString.toString(response ^. responseBody) `shouldBe` "hello"
    it "should get empty json from server" $ do
        response <- Client.get "http://localhost:8000/get-empty-json"
        ByteString.toString(response ^. responseBody) `shouldBe` "{}"
  where
    setUp = do
              forkIO (serve Nothing anApp)
              return ()
