import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

data Status = Complete | Active deriving (Show, Eq)
data TodoItem = Todo String Status deriving (Show, Eq)

complete :: TodoItem -> TodoItem
complete (Todo description status) = (Todo description Complete)

apply :: (TodoItem -> TodoItem) -> TodoItem -> [TodoItem] -> [TodoItem]
-- apply newStatus todo list = [ item | item <- list, item /= todo] ++ [(newStatus todo)]
apply newStatus todo list = fmap (\item -> if item == todo then newStatus item else item) list

shouldCreateTodo = TestCase $
 assertEqual
    "should create a todo item"
        (Todo "something" Active)
        (Todo "something" Active)

shouldAddTodoToList = TestCase $
 assertEqual
    "should add todo to list"
        ([Todo "something" Active])
        ((Todo "something" Active) : [])

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


-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests = hUnitTestToTests $ TestList [
    TestLabel "shouldCreateTodo" shouldCreateTodo,
    TestLabel "shouldAddTodoToList" shouldAddTodoToList,
    TestLabel "shouldMarkTodoAsCompleted" shouldMarkTodoAsCompleted,
    TestLabel "shouldMarkTodoAsCompletedInList" shouldMarkTodoAsCompletedInList
 ]

main = defaultMain tests