{-# LANGUAGE OverloadedStrings #-}
module Database where

import Database.PostgreSQL.Simple

createConnection :: IO Connection
createConnection = connect defaultConnectInfo { connectHost = "localhost", connectPassword = "password" }

extractAllTodos :: IO [(String, String)]
extractAllTodos = do
                    conn <- createConnection
                    result <- (query_ conn "select description, status from todo_list" :: IO [(String, String)])
                    return result