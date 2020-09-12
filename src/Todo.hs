{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Todo where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (msum)
import Control.Monad.Trans.Class (lift)    
import Database.PostgreSQL.Simple
    (Only,  FromRow,
      ToRow,
      Only,
      query,
      query_,
      connect,
      defaultConnectInfo,
      execute_,
      execute,
      ConnectInfo(connectHost, connectPassword),
      Connection )
import Database.PostgreSQL.Simple.FromField ( FromField(..) )
import Database.PostgreSQL.Simple.ToField ( ToField(..) )
import Database.PostgreSQL.Simple.FromRow
    ( FromRow(fromRow), field )
import Database.PostgreSQL.Simple.ToRow      
import Data.Aeson (encode, decode, toJSON, FromJSON, ToJSON)
import Data.ByteString.Lazy.UTF8 (toString, ByteString)
import Data.Char (isSpace)
import qualified Data.List.NonEmpty as N
import Data.Maybe (fromJust, mapMaybe)
import GHC.Generics ( Generic )
import Happstack.Server (askRq, dir, method, decodeBody, path,
    defaultBodyPolicy, takeRequestBody, toResponse, ok, unBody, resp,
    Method(GET, POST, PATCH), ServerPart, Response)
import Data.Int (Int64)
import Database.PostgreSQL.Simple.Types (Only(Only))

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
     
instance ToRow ValidTodoItem where
  toRow (Valid inputTodoItem) = toRow (input_text inputTodoItem, "Active")

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
      "Complete" -> return Complete
      "Active" -> return Active

instance ToField State where
    toField Complete  = toField ("Complete" :: String)
    toField Active = toField ("Active" :: String)      

instance FromRow TodoItem where
    fromRow = TodoItem <$> field <*> field <*> field

--PATCH {id}
data UpdatedTodoItem = UpdatedTodoItem {
  newState :: State
} deriving (Generic, Show, Eq)

instance ToJSON UpdatedTodoItem
instance FromJSON UpdatedTodoItem

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