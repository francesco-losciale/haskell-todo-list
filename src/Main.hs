module Main where

import Happstack.Server (simpleHTTP, nullConf)

import Todo

main :: IO ()
main = simpleHTTP nullConf $ handlers

