{-# LANGUAGE DeriveGeneric #-}
module Todo.TodoValidation where

import GHC.Generics -- used by Aeson
import Data.Aeson as AS

import Data.Char (isSpace)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import qualified Data.Sequence as S

-- This module is replaces Todo module with some more idiomatic code. 

-- please compare the inline comments with the ones in `Todo`

data TodoItem = Todo {
  description :: String,
  state :: Status 
} deriving (Show, Eq, Generic) 

data Status = Complete 
            | Active 
            deriving (Show, Eq, Generic)
            
data TodoError = InvalidDescriptionError 
               | InvalidStatusError 
               deriving (Show, Eq, Generic)

instance FromJSON Status
instance ToJSON Status
instance FromJSON TodoItem
instance ToJSON TodoItem
instance FromJSON TodoError
instance ToJSON TodoError

type TodoValidation = TodoItem -> Maybe TodoError

whenValid :: [TodoValidation] -> TodoItem -> (TodoItem -> a) -> Either (NE.NonEmpty TodoError) a
whenValid validations item = whenValid'
 where
  errors = mapMaybe ($ item) validations
  whenValid' f =
    case errors of
     [] -> {- Valid -} Right $ f item
     (herr:terrs) -> {- Invalid -} Left $ herr NE.:| terrs

-- it makes clear that the item can be added to the list only if
-- the validation passes. 
-- 1. the user has to pass the validation functions necessarily.
-- 2. Either is a pattern that makes clear what can happen calling the function
-- 3. it uses NonEmpty to make it clear in the domain this invariant:
--    `the list of errors can never be empty`
addValidatedTodo :: [TodoValidation] -> TodoItem -> [TodoItem] -> Either (NE.NonEmpty TodoError) [TodoItem]
addValidatedTodo validations item = addValidatedTodo'
 where
  wv = whenValid validations item
  addValidatedTodo' items = wv (:items) 

defaultValidations :: [TodoValidation]
defaultValidations = [validateDescription, validateState]

validateDescription :: TodoItem -> Maybe TodoError
validateDescription item | all isSpace (description item) = Just InvalidDescriptionError
validateDescription _ = Nothing

validateState :: TodoItem -> Maybe TodoError
validateState item =
  case state item of
   Complete -> Just InvalidStatusError
   _ -> Nothing

complete :: TodoItem -> TodoItem
complete (Todo description status) = Todo description Complete

apply :: (TodoItem -> TodoItem) -> TodoItem -> [TodoItem] -> [TodoItem]
apply newStatus todo list = [if item == todo then newStatus item else item | item <- list]

isComplete :: TodoItem -> Bool
isComplete (Todo _ Complete) = True
isComplete (Todo _ _) = False

removeCompleted :: [TodoItem] -> [TodoItem]
removeCompleted = filter (not . isComplete)   