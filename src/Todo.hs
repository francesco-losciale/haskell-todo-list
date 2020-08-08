module Todo (
  addTodo
  , emptyCollection
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

data TodoItem = Todo {
  description :: String,
  state :: Status 
} deriving (Show, Eq) 

data Status = Complete | Active deriving (Show, Eq)

data TodoError = InvalidDescriptionError | InvalidStatusError deriving (Show, Eq)

addTodo :: TodoItem -> [TodoItem] -> [Either TodoError TodoItem]
addTodo item list = concatMap successOrFailure (item:list)         

type TodoValidation = TodoItem -> Maybe TodoError

errorsFor :: TodoValidation -> TodoItem -> [TodoError] 
errorsFor validation list = mapMaybe validation list

allValidationFailures list = errorsFor validateDescription list ++ 
                        errorsFor validateState list

validateDescription :: TodoValidation
validateDescription item | all isSpace (description item) = Just InvalidDescriptionError
                         | otherwise = Nothing   

validateState :: TodoValidation
validateState item | state item == Complete = Just InvalidStatusError
                   | otherwise = Nothing                   

-- The way you've got things organized, I'd probably write the validations as 
-- type TodoValidation = TodoItem -> Maybe TodoError. 
-- Then I'd use something like mapMaybe to get your (possibly empty) list 
-- of failures across all validations for a single item.

-- You should be able to pattern match on that list to produce 
-- the [Either TodoError TodoItem] for each item.

-- At some point you'll use concatMap to generate a 
-- list (of failures or success) for each item in a list and 
-- smoosh them all together into another list.


emptyCollection :: [TodoItem]
emptyCollection = []

complete :: TodoItem -> TodoItem
complete (Todo description status) = Todo description Complete

apply :: (TodoItem -> TodoItem) -> TodoItem -> [TodoItem] -> [TodoItem]
apply newStatus todo list = [if item == todo then newStatus item else item | item <- list]

isComplete :: TodoItem -> Bool
isComplete (Todo _ Complete) = True
isComplete (Todo _ _) = False

removeCompleted :: [TodoItem] -> [TodoItem]
removeCompleted = filter (not . isComplete)
