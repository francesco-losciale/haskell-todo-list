module Main where

import Happstack.Server (simpleHTTP, nullConf)

import Todo
import Server ( handlers )

main :: IO ()
main = simpleHTTP nullConf $ handlers

