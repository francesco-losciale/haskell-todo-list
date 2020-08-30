{-# LANGUAGE DeriveGeneric #-}
module Todo.Todo (
  add
  , errorsFor
  , complete
  , apply
  , isComplete
  , removeCompleted
  , TodoError (..)
  , TodoItem (..)
  , Status (..)
) where

import Data.Char (isSpace)
import Data.Maybe (mapMaybe)
import Data.Aeson as AS
import GHC.Generics -- used by Aeson

-- This module is not used in the codebase now because at some point
-- it was replaced with TodoValidation. 
-- I decided to keep it for learning purposes, so that a more idiomatic
-- version (TodoValidation) can be compared with something less idiomatic.

-- There are comments below that explain what is wrong with these functions.

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

-- there's a separate function with the single responsibility to 
-- retrieve a list of validation errors on an item.

-- 1. the reader needs to understand that an empty list of errors 
-- implies an item is validated.
-- this validation is done elsewhere, in the `add` function. 

-- 2. the functions to validate an item could be defined externally
-- instead of inside the `errorsFor` function.
-- this make the code more customisable in the future

-- please read `TodoValidation` comments now.
errorsFor :: TodoItem -> [TodoError]
errorsFor item = mapMaybe ($ item) checkList
             where
               checkList = [checkDescription, checkState]
               checkDescription item 
                | all isSpace (description item) = Just InvalidDescriptionError
                | otherwise = Nothing
               checkState item 
                | state item == Complete = Just InvalidStatusError
                | otherwise = Nothing   

add :: TodoItem -> [TodoItem] -> [TodoItem]
add item list = if null (errorsFor item) then item:list else list 

complete :: TodoItem -> TodoItem
complete (Todo description status) = Todo description Complete

apply :: (TodoItem -> TodoItem) -> TodoItem -> [TodoItem] -> [TodoItem]
apply newStatus todo list = [if item == todo then newStatus item else item | item <- list]

isComplete :: TodoItem -> Bool
isComplete (Todo _ Complete) = True
isComplete (Todo _ _) = False

removeCompleted :: [TodoItem] -> [TodoItem]
removeCompleted = filter (not . isComplete)
