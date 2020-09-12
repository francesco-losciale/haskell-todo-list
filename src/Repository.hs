{-# LANGUAGE OverloadedStrings #-}
module Repository where 

import Todo

import Data.Int (Int64)
import Database.PostgreSQL.Simple (query_, execute, query, Connection)
import Database.PostgreSQL.Simple.Internal (ConnectInfo(connectHost))
import Database.PostgreSQL.Simple (ConnectInfo(connectPassword))
import Database.PostgreSQL.Simple (defaultConnectInfo)
import Database.PostgreSQL.Simple (connect)
import Database.PostgreSQL.Simple.Types (Only(Only))
import Database.PostgreSQL.Simple (execute_)

write :: ValidTodoItem -> IO Int
write validTodo = do
                    conn <- createConnection
                    let q = "insert into todo_list (description, status) values (?,?) returning id"
                    [xs] <- query conn q validTodo
                    return $ head xs

readTodoList :: IO [TodoItem]
readTodoList = do
                    conn <- createConnection
                    let q = "select id, description, status from todo_list"
                    query_ conn q :: IO [TodoItem]

readTodo :: Int64 -> IO [TodoItem]
readTodo todo_id = do
            conn <- createConnection
            let q = "select id, description, status from todo_list where id = ? "
            (query conn q (Only todo_id)) :: IO [TodoItem]


updateTodo :: Int64 -> UpdatedTodoItem -> IO Int64
updateTodo todo_id updatedTodo = do
                    conn <- createConnection
                    let state = newState updatedTodo
                    execute conn "update todo_list set status = ? where id = ?" (state, todo_id)

deleteTodoList :: IO Int64
deleteTodoList = do
                    conn <- createConnection
                    execute_ conn "delete from todo_list"

createConnection :: IO Connection
createConnection = connect defaultConnectInfo { connectHost = "localhost", connectPassword = "password" }

