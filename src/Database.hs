{-# LANGUAGE OverloadedStrings #-}
module Database where

import Data.Int
import Database.PostgreSQL.Simple

createConnection :: IO Connection
createConnection = connect defaultConnectInfo { connectHost = "localhost", connectPassword = "password" }

extractAllTodos :: IO [(String, String)]
extractAllTodos = do
                    conn <- createConnection
                    result <- (query_ conn "select description, status from todo_list" :: IO [(String, String)])
                    return result

writeAllTodos :: [(String, String)] -> IO Int64
writeAllTodos todoList = do
                    conn <- createConnection
                    executeMany conn
                      "insert into todo_list (description, status) values (?,?)"
                      todoList

deleteAllTodos :: IO Int64
deleteAllTodos = do
                    conn <- createConnection
                    execute_ conn "delete from todo_list"