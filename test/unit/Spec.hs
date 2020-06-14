import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Todo

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

shouldNotAddInvalidTodoToList = TestCase $
 assertEqual
    "should not add invalid todo to list"
        []
        (collect (createList item) item)
 where
    item = (Todo " " Active)

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
    TestLabel "shouldNotAddInvalidTodoToList" shouldNotAddInvalidTodoToList,
    TestLabel "shouldMarkTodoAsCompleted" shouldMarkTodoAsCompleted,
    TestLabel "shouldMarkTodoAsCompletedInList" shouldMarkTodoAsCompletedInList,
    TestLabel "shouldRemoveCompletedTodo" shouldRemoveCompletedTodo
 ]

main = defaultMain tests