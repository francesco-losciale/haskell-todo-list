{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module NewTodo where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (msum)
import Data.ByteString.Lazy.UTF8 (toString, ByteString)
import Data.Aeson (encode, decode, toJSON, FromJSON, ToJSON)
import qualified Data.List.NonEmpty as N
import Data.Maybe (fromJust)
import GHC.Generics ( Generic )
import Happstack.Server (askRq, dir, method, decodeBody, 
    defaultBodyPolicy, takeRequestBody, toResponse, ok, unBody, resp,
    Method(GET, POST), ServerPart, Response)

-- POST
data InputTodoItem = InputTodoItem {
  input_text :: String
} deriving (Generic, Show)

data ValidTodoItem = Valid InputTodoItem 

data TodoError = InvalidDescriptionError 
               | InvalidStatusError 
               deriving (Show, Eq)

type TodoValidation = InputTodoItem -> Maybe TodoError

validate :: [TodoValidation] -> InputTodoItem -> Either (N.NonEmpty TodoError) ValidTodoItem
validate = undefined
     
write :: ValidTodoItem -> Int
write = undefined

--GET
data State = Complete | Active 

data TodoItem = TodoItem {
  todo_id :: Int,
  text :: String,
  state :: State
}

read :: TodoItem
read = undefined

--PUT {id}/complete
data CompleteTodoItem = CompleteTodoItem {
  completed_todo_id :: Int
}

done :: [TodoValidation] -> TodoItem -> CompleteTodoItem
done = undefined

--DELETE {id}
data DeleteTodoItem = DeleteTodoItem {
  deleted_todo_id :: Int
}




-------


instance FromJSON InputTodoItem
instance ToJSON InputTodoItem


handlers :: ServerPart Response
handlers = do 
            decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
            msum [
                dir "todos" $ do 
                    method POST
                    body <- getBody
                    let todo = fromJust $ decode body :: InputTodoItem
                    resp 201 $ toResponse (encode $ todo)
             ]


-- https://stackoverflow.com/questions/8865793/how-to-create-json-rest-api-with-happstack-json-body
-- https://stackoverflow.com/questions/35592415/multiple-monads-in-one-do-block
getBody :: ServerPart ByteString
getBody = do
    req  <- askRq 
    body <- liftIO $ takeRequestBody req 
    case body of 
        Just rqbody -> return . unBody $ rqbody 
        Nothing     -> return "" 
