module ServerSpec where

import Todo
import Test.Hspec

import Happstack.Lite as Server
import Network.Wreq as Client
import Data.ByteString.Lazy.UTF8 as ByteString
import Control.Lens
import Control.Concurrent


anApp :: ServerPart Server.Response
anApp = msum
  [
    dir "hello" $ return (toResponse "hello")
  ]

spec :: Spec
spec = beforeAll (setUp) $ do
  describe "Web Server" $ do
    it "should contact server" $ do
        response <- Client.get "http://localhost:8000/hello"
        ByteString.toString(response ^. responseBody) `shouldBe` "hello"
  where
    setUp = do
              forkIO (serve Nothing anApp)
              return ()
