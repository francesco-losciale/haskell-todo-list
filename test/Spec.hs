import Test.QuickCheck

data Status = Complete | Active deriving (Show, Eq)
type Item = (String, Status)
type TodoList = [Item]

todoListGen :: Gen TodoList
todoListGen = elements [[(['A'..'Z'], Complete), (['A'..'Z'], Active)]]

prop_TodoList_Equality :: Property
prop_TodoList_Equality = forAll todoListGen (\todoList -> todoList == todoList)

-- stack test
main :: IO ()
main = do
     quickCheck prop_TodoList_Equality