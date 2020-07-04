module ServerSpec where

import Todo
import Test.Hspec

import Happstack.Lite
-- http://happstack.com/page/view-page-slug/9/happstack-lite-tutorial
import Network.Wreq
import Control.Lens
-- http://www.serpentine.com/wreq/tutorial.html

import Control.Concurrent

import Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string

anApp :: ServerPart Happstack.Lite.Response
anApp = msum
  [
    dir "hello" $ return (toResponse "hello")
  ]

spec :: Spec
spec = beforeAll (setUp) $ do
  describe "Web Server" $ do
    it "should contact server" $ do
        r <- get "http://localhost:8000/hello"
        (r ^. responseBody) `shouldBe` (BLU.fromString "hello")
  where
    setUp = do
              forkIO (serve Nothing anApp)
              return ()
