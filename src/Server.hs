{-# LANGUAGE OverloadedStrings #-}
{-|
 -Copyright 2016-2016 the openage authors.
 -See copying.md for legal info.
 -}
module Server where

import Data.ByteString.Lazy as BL
import Data.ByteString as B
import Data.Aeson
import System.IO as S
import Control.Concurrent
import Text.Printf
import Control.Monad
import Data.Version
import Network
import Config
import Protocol

myVersion = Version [1, 0, 0] []

startServer :: IO()
startServer = withSocketsDo $ do
  (conf, _) <- loadConf
  port <- getPort conf
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  forever $ do
      (handle, host, port) <- accept sock
      printf "Accepted connection from %s: %s\n" host (show port)
      forkFinally (talk handle) (\_ -> hClose handle)

talk :: S.Handle -> IO()
talk handle = do
  S.hSetNewlineMode handle universalNewlineMode
  S.hSetBuffering handle LineBuffering
  checkVersion handle

getVersion :: S.Handle -> IO ()
getVersion handle = do
  verJson <- B.hGetLine handle
  if (decode . BL.fromStrict) verJson == Just myVersion
    then
      hPrintf handle "Version accepted.\n"
    else do
      hPrintf handle "Incompatible Version.\n"
      hClose handle

getLogin :: S.Handle -> IO ()
getLogin handle = undefined
