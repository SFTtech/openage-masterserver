{-# LANGUAGE OverloadedStrings #-}
{-|
 - Copyright 2016-2016 the openage authors.
 - See copying.md for legal info.
 -}
{-|main entry file for the openage masterserver
 - this server will listen on a tcp socket
 - and provide a funny API for gameservers and clients
 - to start communicating with each other.
 -}
module Main where
import Server


main :: IO ()
main = startServer
