module Todo (
  --   validate
  -- , collect
  -- , 
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

data TodoItem = Todo {
  description :: String,
  state :: Status 
} deriving (Show, Eq) 

data Status = Complete | Active deriving (Show, Eq)

data TodoError = InvalidDescriptionError deriving (Show, Eq)

addTodo :: TodoItem -> [TodoItem] -> [Either TodoError TodoItem]
addTodo item list = fmap validate (item : list)
                    where 
                      validate item = if all isSpace (description item) then
                        Left InvalidDescriptionError
                      else 
                        Right item

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
