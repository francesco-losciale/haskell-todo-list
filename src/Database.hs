{-# LANGUAGE OverloadedStrings #-}
module Database (
    extractAllTodos
  -- , writeAllTodos
  , extractList
  , wantToWrite
  , toTodoItem
  , writeTodo
  , deleteAllTodos
) where

import Todo.TodoValidation
import Data.Int
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField

data TodoItemWrite = TodoItemWrite  {
  description :: String,
  state :: Status 
} deriving (Show, Eq) 

wantToWrite :: TodoItem -> TodoItemWrite 
wantToWrite (Todo id desc state) = (TodoItemWrite desc state)

data TodoItemRead = TodoItemRead  {
  readTodo_id :: Int,
  readDescription :: String,
  readState :: Status 
} deriving (Show, Eq) 

toTodoItem :: TodoItemRead -> TodoItem
toTodoItem (TodoItemRead id desc state) = (Todo id desc state)

instance FromField Status where
  fromField field mdata = do
    value <- fromField field mdata
    case value :: String of
      "Complete" -> return Complete
      "Active" -> return Active

instance ToField Status where
    toField Complete  = toField ("Complete" :: String)
    toField Active = toField ("Active" :: String)

instance FromRow TodoItemRead where
    fromRow = TodoItemRead <$> field <*> field <*> field

instance ToRow TodoItemWrite where
  toRow (TodoItemWrite desc state) = toRow (desc, state)

createConnection :: IO Connection
createConnection = connect defaultConnectInfo { connectHost = "localhost", connectPassword = "password" }

extractAllTodos :: IO [TodoItem]
extractAllTodos = undefined
-- extractAllTodos = do
--                     conn <- createConnection
--                     query_ conn "select id, description, status from todo_list" :: IO [TodoItem]

extractList :: IO [TodoItemRead]
extractList = do
                    conn <- createConnection
                    query_ conn "select id, description, status from todo_list" :: IO [TodoItemRead]


writeAllTodos :: [TodoItem] -> IO Int64
writeAllTodos = undefined
-- writeAllTodos todoList = do
--                     conn <- createConnection
--                     executeMany conn
--                       "insert into todo_list (description, status) values (?,?) returning id"
--                       todoList

writeTodo :: TodoItemWrite -> IO Int64
writeTodo todo = do
                    conn <- createConnection
                    let q = "insert into todo_list (description, status) values (?,?) returning id"
                    [xs] <- query conn q todo
                    return $ head xs

deleteAllTodos :: IO Int64
deleteAllTodos = do
                    conn <- createConnection
                    execute_ conn "delete from todo_list"