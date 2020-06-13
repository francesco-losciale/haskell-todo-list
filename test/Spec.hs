import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.Char (isSpace)

data Status = Complete | Active deriving (Show, Eq)
data TodoItem = Todo String Status deriving (Show, Eq) -- TODO: adding a field requires refactoring. how to fix it?
data TodoError = InvalidDescriptionError deriving (Show, Eq)

-- item

validate :: TodoItem -> (Either TodoError TodoItem)
validate (Todo description status) = if all isSpace description then Left InvalidDescriptionError else Right (Todo description status)

createList :: TodoItem -> [TodoItem]
createList item = collect [] item

collect :: [TodoItem] -> TodoItem -> [TodoItem]
collect list item = mappend list [item]


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

-- tests


shouldCreateTodo = TestCase $
 assertEqual
    "should create a todo item"
        (Todo "something" Active)
        (Todo "something" Active)

shouldTodoValidationFail = TestCase $
 assertEqual
    "should validation of empty description fail"
        (validate (Todo "" Active))
        (Left InvalidDescriptionError)

shouldCreateTodoToList = TestCase $
 assertEqual
    "should create todo to list"
        [item]
        (createList item)
 where
    item = (Todo "something" Active)

shouldAddTodoToList = TestCase $
 assertEqual
    "should add todo to list"
        [item, item]
        (collect (createList item) item)
 where
    item = (Todo "something" Active)

shouldMarkTodoAsCompleted = TestCase $
 assertEqual
    "given a todo then mark it as complete"
        (Todo "something" Complete)
        (complete (Todo "something" Active))

shouldMarkTodoAsCompletedInList = TestCase $
 assertEqual
    "given a todo list then mark a specific one as complete"
        expectedList
        (apply complete todo list)
 where
    todo = (Todo "something2" Active)
    list = [(Todo "something1" Active), (Todo "something2" Active), (Todo "something3" Active)]
    expectedList = [(Todo "something1" Active), (Todo "something2" Complete), (Todo "something3" Active)]

shouldRemoveCompletedTodo = TestCase $
 assertEqual
    "given a todo list then remove all the completed ones"
        expectedList
        (removeCompleted list)
 where
    list = [(Todo "something1" Active), (Todo "something2" Active), (Todo "something3" Complete)]
    expectedList = [(Todo "something1" Active), (Todo "something2" Active)]


-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests = hUnitTestToTests $ TestList [
    TestLabel "shouldCreateTodo" shouldCreateTodo,
    TestLabel "shouldTodoValidationFail" shouldTodoValidationFail,
    TestLabel "shouldCreateTodoToList" shouldCreateTodoToList,
    TestLabel "shouldAddTodoToList" shouldAddTodoToList,
    TestLabel "shouldMarkTodoAsCompleted" shouldMarkTodoAsCompleted,
    TestLabel "shouldMarkTodoAsCompletedInList" shouldMarkTodoAsCompletedInList,
    TestLabel "shouldRemoveCompletedTodo" shouldRemoveCompletedTodo
 ]

main = defaultMain tests