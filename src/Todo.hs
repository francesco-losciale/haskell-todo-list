module Todo (
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

data TodoItem = Todo {
  description :: String,
  state :: Status 
} deriving (Show, Eq) 

data Status = Complete | Active deriving (Show, Eq)
data TodoError = InvalidDescriptionError | InvalidStatusError deriving (Show, Eq)

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
