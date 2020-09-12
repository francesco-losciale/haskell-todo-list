module Main where

import Happstack.Server (simpleHTTP, nullConf)

import NewTodo

main :: IO ()
main = simpleHTTP nullConf $ handlers

