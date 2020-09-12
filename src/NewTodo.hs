{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module NewTodo where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (msum)
import Control.Monad.Trans.Class (lift)    
import Database.PostgreSQL.Simple
    ( FromRow,
      ToRow,
      query,
      query_,
      connect,
      defaultConnectInfo,
      execute_,
      ConnectInfo(connectHost, connectPassword),
      Connection )
import Database.PostgreSQL.Simple.FromField ( FromField(..) )
import Database.PostgreSQL.Simple.ToRow      
import Data.Aeson (encode, decode, toJSON, FromJSON, ToJSON)
import Data.ByteString.Lazy.UTF8 (toString, ByteString)
import Data.Char (isSpace)
import qualified Data.List.NonEmpty as N
import Data.Maybe (fromJust, mapMaybe)
import GHC.Generics ( Generic )
import Happstack.Server (askRq, dir, method, decodeBody, path,
    defaultBodyPolicy, takeRequestBody, toResponse, ok, unBody, resp,
    Method(GET, POST), ServerPart, Response)
import Data.Int (Int64)

-- POST
data InputTodoItem = InputTodoItem {
  input_text :: String
} deriving (Generic, Show)

data ValidTodoItem = Valid InputTodoItem deriving (Generic, Show)

data TodoError = InvalidDescriptionError 
               | InvalidStatusError 
               deriving (Generic, Show, Eq)

type TodoValidation = InputTodoItem -> Maybe TodoError

validate :: [TodoValidation] -> InputTodoItem -> Either (N.NonEmpty TodoError) ValidTodoItem
validate validations todo = do 
    let errors = mapMaybe ($ todo) validations
    case errors of 
        [] -> Right $ Valid todo
        (herr:terrs) -> Left $ herr N.:| terrs
     
write :: ValidTodoItem -> IO Int
write validTodo = do
                    conn <- createConnection
                    let q = "insert into todo_list (description, status) values (?,?) returning id"
                    [xs] <- query conn q validTodo
                    return $ head xs

createConnection :: IO Connection
createConnection = connect defaultConnectInfo { connectHost = "localhost", connectPassword = "password" }


instance ToRow ValidTodoItem where
  toRow (Valid inputTodoItem) = toRow (input_text inputTodoItem, "active")

--GET
data State = Complete | Active deriving (Generic, Show, Eq)

data TodoItem = TodoItem {
  todo_id :: Int,
  text :: String,
  state :: State
} deriving (Generic, Show, Eq)

instance FromJSON TodoItem
instance FromJSON State

instance FromField State where
  fromField field mdata = do
    value <- fromField field mdata
    case value :: String of
      "complete" -> return Complete
      "active" -> return Active

read :: TodoItem
read = undefined

--PATCH {id}
data UpdatedTodoItem = UpdatedTodoItem {
  newState :: State
} deriving (Generic, Show, Eq)

instance ToJSON UpdatedTodoItem

done :: [TodoValidation] -> TodoItem -> UpdatedTodoItem
done = undefined

--DELETE {id}
data DeleteTodoItem = DeleteTodoItem {
  deleted_todo_id :: Int
}




-------


instance FromJSON InputTodoItem
instance ToJSON InputTodoItem

instance FromJSON ValidTodoItem
instance ToJSON ValidTodoItem

instance FromJSON TodoError
instance ToJSON TodoError

instance ToJSON TodoItem
instance ToJSON State

handlers :: ServerPart Response
handlers = do 
            decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
            msum [
                dir "todos" $ do 
                    method POST
                    body <- getBody
                    let todo = fromJust $ decode body :: InputTodoItem
                    case (validate defaultValidations todo) of 
                        Left errors -> resp 400 $ toResponse (encode $ ErrorsPayload {item = todo, errors = errors})
                        Right validTodo -> do 
                                    id <- lift $ write validTodo
                                    resp 201 $  toResponse (encode $ id),
                dir "todos" $ path $ \(id :: Int64) ->  do method GET 
                                                           ok (toResponse $ encode id),
                dir "todos" $ do method GET 
                                 ok (toResponse $ encode [TodoItem {todo_id=1, text ="example", state = Active }])
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


defaultValidations :: [TodoValidation]
defaultValidations = [validateDescription]

validateDescription :: InputTodoItem -> Maybe TodoError
validateDescription item | all isSpace (input_text item) = Just InvalidDescriptionError
validateDescription _ = Nothing

data InvalidItemPayload = ErrorsPayload
  { item :: InputTodoItem,
    errors :: N.NonEmpty TodoError
  }
  deriving (Generic, Show)

instance FromJSON InvalidItemPayload
instance ToJSON InvalidItemPayload