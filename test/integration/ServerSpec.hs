{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module ServerSpec where

import Todo
import Test.Hspec

import Happstack.Lite as Server
import Network.Wreq as Client
import Data.ByteString.Lazy.UTF8 as ByteString
import Control.Lens
import Control.Concurrent

import Data.Aeson as A
import Data.Data
import Data.Maybe
import Happstack.Server.Types
import Control.Monad.IO.Class (liftIO)

anApp :: ServerPart Server.Response
anApp = msum
  [
    Server.dir "hello" $ return (toResponse "hello"),
    Server.dir "get-empty-json" $ return (toResponse "{}"),
    Server.dir "get-unit" $ return (toResponse "{\"x\":1,\"y\":2}")
  ]

data Unit = Unit { x :: Int, y :: Int } deriving (Show, Eq, Data, Typeable)
instance FromJSON Unit where
    parseJSON = withObject "Unit" $ \v -> Unit
        <$> v .: "x"
        <*> v .: "y"

instance ToJSON Unit where
    -- this generates a Value
    toJSON (Unit x y) =
        object ["x" A..= x, "y" A..= y]

    -- this encodes directly to a bytestring Builder
    toEncoding (Unit x y) =
        pairs ("x" A..= x <> "y" A..= y)

spec :: Spec
spec = beforeAll (setUp) $ do
  describe "Web Server" $ do
    it "should contact server" $ do
        response <- Client.get "http://localhost:8000/hello"
        ByteString.toString(response ^. responseBody) `shouldBe` "hello"
    it "should get empty json from server" $ do
        response <- Client.get "http://localhost:8000/get-empty-json"
        ByteString.toString(response ^. responseBody) `shouldBe` "{}"
    it "should decode json " $ do
        response <- Client.get "http://localhost:8000/get-unit"
        let unit = fromJust $ A.decode (response ^. responseBody) :: Unit
        unit `shouldBe` (Unit {x=1,y=2})
    it "should encode to json " $ do
        let unit = (Unit {x=1,y=2})
        (A.encode unit) `shouldBe` "{\"x\":1,\"y\":2}"
  where
    setUp = do
              forkIO (serve Nothing anApp)
              return ()
