module Temp where

import Todo

import Data.Char (isSpace)
import Data.List.NonEmpty
import Data.Maybe (mapMaybe)
import Data.Sequence

type TodoValidation = TodoItem -> Maybe TodoError

whenValid :: [TodoValidation] -> TodoItem -> (TodoItem -> a) -> Either (NonEmpty TodoError) a
whenValid validations item = whenValid'
 where
  errors = mapMaybe ($ item) validations
  whenValid' f =
    case errors of
     [] -> {- Valid -} Right $ f item
     (herr:terrs) -> {- Invalid -} Left % herr :< terrs

addValidatedTodo :: [TodoValidation] -> TodoItem -> [TodoItem] -> Either (NonEmpty TodoError) [TodoItem]
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