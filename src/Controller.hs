{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
module Controller where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (msum)
import Data.Aeson (encode, decode, toJSON)
import Data.ByteString.Lazy.UTF8 (toString, ByteString)
import Data.Maybe (fromJust)
import Happstack.Server (askRq, dir, method, decodeBody, 
    defaultBodyPolicy, takeRequestBody, toResponse, ok, unBody, resp,
    Method(GET, POST), ServerPart, Response)

import Database (extractAllTodos)
import Todo.TodoValidation (defaultValidations, addValidatedTodo, TodoItem(..), Status(..))

-- https://stackoverflow.com/questions/8865793/how-to-create-json-rest-api-with-happstack-json-body
-- https://stackoverflow.com/questions/35592415/multiple-monads-in-one-do-block
getBody :: ServerPart ByteString
getBody = do
    req  <- askRq 
    body <- liftIO $ takeRequestBody req 
    case body of 
        Just rqbody -> return . unBody $ rqbody 
        Nothing     -> return "" 

handlers :: ServerPart Response
handlers = do 
            decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
            msum [
                dir "health" $ do method GET 
                                  ok (toResponse "success"),
                dir "todos" $ do method POST
                                 body <- getBody
                                 let todo = fromJust $ decode body :: TodoItem
                                 list <- extractAllTodos
                                 resp 201 $ toResponse (encode todo), 
                dir "todos" $ do method GET 
                                 ok (toResponse $ encode [(Todo "example" Active)])                                 
             ]
