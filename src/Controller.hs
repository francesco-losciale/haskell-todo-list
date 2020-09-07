{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Controller where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (msum)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (encode, decode, toJSON, FromJSON, ToJSON)
import Data.ByteString.Lazy.UTF8 (toString, ByteString)
import Data.List.NonEmpty 
import Data.Maybe (fromJust)
import Data.Map (singleton, insert)
import GHC.Generics ( Generic )
import Happstack.Server (askRq, dir, method, decodeBody, 
    defaultBodyPolicy, takeRequestBody, toResponse, ok, unBody, resp,
    Method(GET, POST), ServerPart, Response)

import Database (extractAllTodos)
import Todo.TodoValidation (defaultValidations, addValidatedTodo, TodoItem(..), TodoError(..), Status(..))


data CreatedItemPayload = ResponsePayload
  { newTodoId :: Int,
    list :: [TodoItem]
  }
  deriving (Generic, Show)

instance FromJSON CreatedItemPayload
instance ToJSON CreatedItemPayload

data InvalidItemPayload = ErrorsPayload
  { item :: TodoItem,
    errors :: NonEmpty TodoError
  }
  deriving (Generic, Show)

instance FromJSON InvalidItemPayload
instance ToJSON InvalidItemPayload

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
                                 list <- lift extractAllTodos
                                 let todo = fromJust $ decode body :: TodoItem
                                 case addValidatedTodo defaultValidations todo list of
                                    Left err -> resp 400 $ toResponse (encode $ ErrorsPayload {item = todo, errors = err} )
                                    Right list -> resp 201 $ toResponse (encode $ ResponsePayload { newTodoId = 123, list = list}),
                dir "todos" $ do method GET 
                                 ok (toResponse $ encode [(Todo "example" Active)])                                 
             ]
            where 
                collectErrors errors = errors
                newList items = items
