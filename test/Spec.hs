import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

data Status = Complete | Active deriving (Show, Eq)
data TodoItem = Todo String Status deriving (Show, Eq)

complete :: TodoItem -> TodoItem
complete (Todo description status) = (Todo description Complete)

shouldCreateTodo = TestCase $
 assertEqual
    "should create a todo item"
        (Todo "something" Active)
        (Todo "something" Active)

shouldAddTodoToList = TestCase $
 assertEqual
    "should add todo to list"
        ((Todo "something" Active) : [])
        ([Todo "something" Active])

shouldMarkTodoAsCompleted = TestCase $
 assertEqual
    "given a todo then mark it as complete"
        (complete (Todo "something" Active))
        (Todo "something" Complete)


-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests = hUnitTestToTests $ TestList [
    TestLabel "shouldCreateTodo" shouldCreateTodo,
    TestLabel "shouldAddTodoToList" shouldAddTodoToList,
    TestLabel "shouldMarkTodoAsCompleted" shouldMarkTodoAsCompleted
 ]

main = defaultMain tests