{-# LANGUAGE ExtendedDefaultRules #-}
module ServerSpec where

import Todo.TodoValidation 

import Test.Hspec

import Data.ByteString.Lazy.UTF8 as BS
import Control.Lens as LENS
import Data.Aeson as AS
import Control.Monad (msum)

import Control.Concurrent

-- https://hackage.haskell.org/package/wreq-0.5.3.2/docs/Network-Wreq.html
import Network.Wreq as CLIENT 
import Network.HTTP.Types.Status
-- Crash course on Happstack - http://happstack.com/docs/crashcourse/index.html
import Happstack.Server (
  nullConf, 
  simpleHTTP, 
  toResponse, 
  ok, 
  dir, 
  method, 
  Method(..))

main :: IO ()
main = simpleHTTP nullConf $ msum
  [
    dir "health" $ do method GET 
                      ok (toResponse "hello")
  ]

spec :: Spec
spec = beforeAll (setUp) $ do
  describe "Web Server" $ do
    it "should contact server" $ do
        response <- CLIENT.get "http://localhost:8000/health"
        BS.toString(response LENS.^. responseBody) `shouldBe` "hello" 
    it "should return 200 healthy" $ do
        response <- CLIENT.get "http://localhost:8000/health"
        response LENS.^. responseStatus `shouldBe` ok200
      
  where
    setUp = do forkIO main
               return () 
