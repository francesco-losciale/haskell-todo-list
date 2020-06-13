module Todo where
import Data.Char (isSpace)

data Status = Complete | Active deriving (Show, Eq)
data TodoItem = Todo String Status deriving (Show, Eq) -- TODO: adding a field requires refactoring. how to fix it?
data TodoError = InvalidDescriptionError deriving (Show, Eq)

-- utils

validate :: TodoItem -> (Either TodoError TodoItem)
validate (Todo description status) = if all isSpace description then Left InvalidDescriptionError else Right (Todo description status)

createList :: TodoItem -> [TodoItem]
createList item = collect [] item

collect :: [TodoItem] -> TodoItem -> [TodoItem]
collect list item = case validate item of
                        Left _ -> list
                        Right item -> mappend list [item]
-- Either returns only first error, is ok for now. we'll not handle it.
-- TODO: improve using https://hackage.haskell.org/package/validation



-- status management

complete :: TodoItem -> TodoItem
complete (Todo description status) = (Todo description Complete)

apply :: (TodoItem -> TodoItem) -> TodoItem -> [TodoItem] -> [TodoItem]
apply newStatus todo list = [if item == todo then newStatus item else item | item <- list]

isComplete :: TodoItem -> Bool
isComplete (Todo _ Complete) = True
isComplete (Todo _ _) = False

removeCompleted :: [TodoItem] -> [TodoItem]
removeCompleted = filter (not . isComplete)
