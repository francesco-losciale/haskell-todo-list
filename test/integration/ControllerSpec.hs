module ControllerSpec where

import Control.Concurrent (forkIO)
import Control.Lens 
import Control.Monad (msum)
import Data.ByteString.Lazy.UTF8 (toString)
import Happstack.Server (dir, method, simpleHTTP, nullConf, decodeBody, defaultBodyPolicy, toResponse, ok, Method(GET,POST), ServerPart, Response)
import Network.Wreq (get, responseBody)
import Test.Hspec 

handlers :: ServerPart Response
handlers = do 
            decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
            msum [
                dir "health" $ do method GET 
                                  ok (toResponse "hello")
             ]

main :: IO ()
main = simpleHTTP nullConf $ handlers

spec :: Spec
spec = beforeAll (setUp) $ do
  describe "Controller" $ do
    it "should be healthy" $ do
        response <- get "http://localhost:8000/health"
        toString(response ^. responseBody) `shouldBe` "hello" 
  where
    setUp = do forkIO main
               return () 