module Todo.TodoValidation where

import Data.Char (isSpace)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import qualified Data.Sequence as S

-- Similar module to Todo, with more idiomatic haskell

data TodoItem = Todo {
  description :: String,
  state :: Status 
} deriving (Show, Eq) 

data Status = Complete | Active deriving (Show, Eq)
data TodoError = InvalidDescriptionError | InvalidStatusError deriving (Show, Eq)

type TodoValidation = TodoItem -> Maybe TodoError

whenValid :: [TodoValidation] -> TodoItem -> (TodoItem -> a) -> Either (NE.NonEmpty TodoError) a
whenValid validations item = whenValid'
 where
  errors = mapMaybe ($ item) validations
  whenValid' f =
    case errors of
     [] -> {- Valid -} Right $ f item
     (herr:terrs) -> {- Invalid -} Left $ herr NE.:| terrs

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