module Main where

import Happstack.Server (simpleHTTP, nullConf)

import Controller

main :: IO ()
main = simpleHTTP nullConf $ handlers

