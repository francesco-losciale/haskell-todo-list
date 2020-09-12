{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Todo where

import Data.Aeson.Types (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Data.List.NonEmpty( NonEmpty( (:|) ) )
import Database.PostgreSQL.Simple.ToRow (ToRow(toRow))
import Database.PostgreSQL.Simple.FromField (FromField(fromField))
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import Database.PostgreSQL.Simple.FromRow (field, FromRow(fromRow))
import Data.Maybe (mapMaybe)
import Data.Char (isSpace)

-- todo as coming from client
data InputTodoItem = InputTodoItem {
  input_text :: String
} deriving (Generic, Show)

-- item after validation (this only can be persisted with `write`)
data ValidTodoItem = Valid InputTodoItem deriving (Generic, Show)

-- possible validation errors
data TodoError = InvalidDescriptionError | InvalidStatusError deriving (Generic, Show, Eq)

-- defines a validation function type
type TodoValidation = InputTodoItem -> Maybe TodoError

-- validate function restricts the type of validation function
validate :: [TodoValidation] -> InputTodoItem -> Either (NonEmpty TodoError) ValidTodoItem
validate validations todo = do 
    let errors = mapMaybe ($ todo) validations
    case errors of 
        [] -> Right $ Valid todo
        (herr:terrs) -> Left $ herr :| terrs
     
-- todo can be active or complete     
data State = Complete | Active deriving (Generic, Show, Eq)

-- represents a todo item after it has been validated and persisted
data TodoItem = TodoItem {
  todo_id :: Int,
  text :: String,
  state :: State
} deriving (Generic, Show, Eq)

-- represents the data that can be updated with PATCH 
data UpdatedTodoItem = UpdatedTodoItem {
  newState :: State
} deriving (Generic, Show, Eq)

-- how errors are returned to the front end
data InvalidItemPayload = InvalidItemPayload
  { item :: InputTodoItem,
    errors :: NonEmpty TodoError
  }
  deriving (Generic, Show)

defaultValidations :: [TodoValidation]
defaultValidations = [validateDescription]

validateDescription :: InputTodoItem -> Maybe TodoError
validateDescription item | all isSpace (input_text item) = Just InvalidDescriptionError
validateDescription _ = Nothing

instance ToRow ValidTodoItem where
  toRow (Valid inputTodoItem) = toRow (input_text inputTodoItem, "Active")

instance FromJSON TodoItem
instance ToJSON TodoItem
instance FromRow TodoItem where
    fromRow = TodoItem <$> field <*> field <*> field

instance FromJSON State
instance ToJSON State
instance FromField State where
  fromField field mdata = do
    value <- fromField field mdata
    case value :: String of
      "Complete" -> return Complete
      "Active" -> return Active
instance ToField State where
    toField Complete  = toField ("Complete" :: String)
    toField Active = toField ("Active" :: String)      

instance ToJSON UpdatedTodoItem
instance FromJSON UpdatedTodoItem

instance FromJSON InputTodoItem
instance ToJSON InputTodoItem

instance FromJSON ValidTodoItem
instance ToJSON ValidTodoItem

instance FromJSON TodoError
instance ToJSON TodoError

instance FromJSON InvalidItemPayload
instance ToJSON InvalidItemPayload