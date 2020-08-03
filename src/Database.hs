{-# LANGUAGE OverloadedStrings #-}
module Database where

import Todo
import Data.Int
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField

instance FromField Status where
  fromField field mdata = do
    value <- fromField field mdata
    case value :: String of
      "Complete" -> return Complete
      "Active" -> return Active

instance ToField Status where
    toField Complete  = toField ("Complete" :: String)
    toField Active = toField ("Active" :: String)

instance FromRow TodoItem where
    fromRow = Todo <$> field <*> field

instance ToRow TodoItem where
  toRow (Todo description status) = toRow (description, status)

createConnection :: IO Connection
createConnection = connect defaultConnectInfo { connectHost = "localhost", connectPassword = "password" }

extractAllTodos :: IO [TodoItem]
extractAllTodos = do
                    conn <- createConnection
                    query_ conn "select description, status from todo_list" :: IO [TodoItem]

writeAllTodos :: [TodoItem] -> IO Int64
writeAllTodos todoList = do
                    conn <- createConnection
                    executeMany conn
                      "insert into todo_list (description, status) values (?,?)"
                      todoList

deleteAllTodos :: IO Int64
deleteAllTodos = do
                    conn <- createConnection
                    execute_ conn "delete from todo_list"