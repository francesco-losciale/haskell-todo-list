{-# LANGUAGE OverloadedStrings #-}
module Database where

import Database.PostgreSQL.Simple

createConnection :: IO Connection
createConnection = connect defaultConnectInfo { connectHost = "localhost", connectPassword = "password" }

testQuery :: String -> IO Int
testQuery sql = do
                    conn <- createConnection
                    [Only i] <- (query_ conn "select 1" :: IO [Only Int])
                    return i