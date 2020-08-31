{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
module ServerSpec where

import Todo.TodoValidation 

import Test.Hspec

import Data.ByteString.Lazy.UTF8 as BS
import Control.Lens as LENS
import Data.Aeson as AS
import Control.Monad (msum)
import Data.Maybe as MB

import Control.Concurrent

-- https://hackage.haskell.org/package/wreq-0.5.3.2/docs/Network-Wreq.html
-- https://stackoverflow.com/questions/8865793/how-to-create-json-rest-api-with-happstack-json-body
import Network.Wreq as CLIENT hiding (Response)
import Network.HTTP.Types.Status
-- Crash course on Happstack - http://happstack.com/docs/crashcourse/index.html
import Happstack.Server (
  nullConf, 
  simpleHTTP, 
  toResponse, 
  ok, 
  dir, 
  look,
  askRq,
  method, 
  decodeBody,
  body,
  unBody,
  defaultBodyPolicy,
  takeRequestBody,
  badRequest,
  ServerPart, 
  Response,
  Method(..))
import Happstack.Server.RqData (RqData, look, getDataFn)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = simpleHTTP nullConf $ handlers

handlers :: ServerPart Response
handlers =
    do decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
       msum [ 
         dir "health" $ do method GET 
                           ok (toResponse "hello"),
        --  dir "todos" $ do method POST 
        --                   todo <- body $ look "id"
        --                   ok $ toResponse ("done"),
        --  helloPart,
         helloJson                          
        ]

helloPart :: ServerPart Response
helloPart =
    do r <- getDataFn (body $ look "hello")
       case r of
         (Left e) ->
             badRequest $ toResponse (unlines e)
         (Right sth) ->
             ok $ toResponse ("done")  


-- put this function in a library somewhere
getBody :: ServerPart BS.ByteString
getBody = do
    req  <- askRq 
    body <- liftIO $ takeRequestBody req 
    case body of 
        Just rqbody -> return . unBody $ rqbody 
        Nothing     -> return "" 

helloJson :: ServerPart Response
helloJson =
      do body <- getBody -- it's a ByteString
         let todo = MB.fromJust $ AS.decode body :: TodoItem -- how to parse json
         ok $ toResponse $ AS.encode todo -- how to send json back. 
    -- do r <- getDataFn (body $ look "json")
    --    case r of
    --      (Left e) ->
    --          badRequest $ toResponse (unlines e)
    --      (Right sth) ->
    --          ok $ toResponse ("done")  

spec :: Spec
spec = beforeAll (setUp) $ do
  describe "Web Server" $ do
    it "should contact server" $ do
        response <- CLIENT.get "http://localhost:8000/health"
        BS.toString(response LENS.^. responseBody) `shouldBe` "hello" 
    it "should return 200 healthy" $ do
        response <- CLIENT.get "http://localhost:8000/health"
        response LENS.^. responseStatus `shouldBe` ok200
    -- it "should return 200 when POST todo" $ do
    --     response <- CLIENT.post "http://localhost:8000/todos" (CLIENT.partText "hello" "world") --(AS.toJSON "id")
    --     BS.toString(response LENS.^. responseBody) `shouldBe` "done"      
    it "should return 200 when POST todo" $ do
        response <- CLIENT.post "http://localhost:8000/todos" (AS.toJSON (Todo "t" Active))
        BS.toString(response LENS.^. responseBody) `shouldBe` "{\"state\":\"Active\",\"description\":\"t\"}"      
  where
    setUp = do forkIO main
               return () 
