{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |Copyright 2016-2016 the openage authors.
-- See copying.md for legal info.
--
-- main entry file for the openage masterserver
-- this server will listen on a tcp socket
-- and provide a funny API for gameservers and clients
-- to start communicating with each other.
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception.Base (finally)
import Control.Monad
import Data.Aeson
import Data.ByteString as B
import Data.ByteString.Lazy as BL
import Data.List as L
import Data.Map.Strict as Map
import Data.Maybe
import Data.Version (makeVersion)
import Database.Persist
import Network
import Text.Printf
import System.IO as S

import Config
import Server
import Protocol
import DBSchema

main :: IO ()
main = withSocketsDo $ do
  (conf, _) <- loadConf
  port <- getPort conf
  server <- newServer
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  forever $ do
      (handle, host, clientPort) <- accept sock
      printf "Accepted connection from %s: %s\n" host (show clientPort)
      forkFinally (talk handle server) (\_ ->
        printf "Connection from %s closed\n" host >> hClose handle)

talk :: Handle -> Server -> IO()
talk handle server = do
  S.hSetNewlineMode handle universalNewlineMode
  S.hSetBuffering handle LineBuffering
  checkVersion handle
  mayClient <- checkAddClient server handle
  case mayClient of
    Just client@Client{..} -> do
      sendMessage handle "Login success."
      runClient server client `finally` removeClient server clientName
    Nothing ->
      sendError handle "Login failed."

-- |Compare Version to own
checkVersion :: S.Handle -> IO ()
checkVersion handle = do
  verJson <- B.hGetLine handle
  (conf, _) <- loadConf
  myVersion <- getVersion conf
  if (peerProtocolVersion .
      fromJust .
      decode .
      BL.fromStrict) verJson == makeVersion myVersion
    then
      sendMessage handle "Version accepted."
    else do
      sendError handle "Incompatible Version."
      thread <- myThreadId
      killThread thread

-- |Get login credentials from handle, add client to server
-- clientmap and return Client
checkAddClient :: Server -> Handle -> IO(Maybe Client)
checkAddClient Server{..} handle = do
  loginJson <- B.hGetLine handle
  case (decode . BL.fromStrict) loginJson of
    Just Login{..} -> do
      Just (Entity _ Player{..}) <- getPlayer loginName
      if loginPassword == playerPassword
        then do
          clientMap <- readTVarIO clients
          client <- newClient playerUsername handle
          if member playerUsername clientMap
            then do
              sendChannel (clientMap!playerUsername) Logout
              atomically $ writeTVar clients
                $ Map.insert playerUsername client clientMap
              return $ Just client
            else atomically $ do
              writeTVar clients
                $ Map.insert playerUsername client clientMap
              return $ Just client
        else return Nothing
    _ -> do
      sendError handle "Unknown Format."
      return Nothing

-- |Runs individual Client
runClient :: Server -> Client -> IO ()
runClient server@Server{..} client@Client{..} = do
  _ <- race internalReceive $ mainLoop server client
  return ()
    where
      internalReceive = forever $ do
        msg <- B.hGetLine clientHandle
        case (decode . BL.fromStrict) msg of
          Just mess -> sendChannel client mess
          Nothing -> sendError clientHandle "Could not read message."

-- |Remove Client from servers client map and close his games
removeClient :: Server -> AuthPlayerName -> IO ()
removeClient server@Server{..} clientName = do
  clientLis <- readTVarIO clients
  let client = clientLis!clientName
  case clientInGame client of
    Just game -> do
      leaveGame server client game
      cleanTvar
    Nothing ->
      cleanTvar
  where
    cleanTvar = atomically $
      modifyTVar' clients $ Map.delete clientName

-- |Main Lobby loop with ClientMessage Handler functions
mainLoop :: Server -> Client -> IO ()
mainLoop server@Server{..} client@Client{..} = do
  msg <- atomically $ readTChan clientChan
  case msg of
    GameQuery -> do
      gameLis <- getGameList server
      sendGameQueryAnswer clientHandle gameLis
      mainLoop server client
    GameInit{..} -> do
      maybeGame <- checkAddGame server clientName msg
      case maybeGame of
        Just Game{..} -> do
          gameLis <- readTVarIO games
          clientLis <- readTVarIO clients
          atomically $ writeTVar clients
            $ Map.adjust (addClientGame gameName) clientName clientLis
          atomically $ writeTVar games
            $ Map.adjust (joinPlayer clientName True) gameName gameLis
          sendMessage clientHandle "Added game."
          gameLoop server client gameInitName
        Nothing -> do
          sendError clientHandle "Failed adding game."
          mainLoop server client
    GameJoin{..} -> do
      success <- joinGame server client gameId
      if success
        then gameLoop server client gameId
        else mainLoop server client
    Logout ->
      sendMessage clientHandle "You have been logged out."
    _ -> do
      sendError clientHandle "Unknown Message."
      mainLoop server client

-- |Gamestate loop
gameLoop :: Server -> Client -> GameName -> IO ()
gameLoop server@Server{..} client@Client{..} game= do
  msg <- atomically $ readTChan clientChan
  case msg of
    GameStart -> do
      gameLis <- readTVarIO games
      if clientName == gameHost (gameLis!game)
        then
          if L.all parReady $ gamePlayers $ gameLis!game
            then do
              broadcastGame server game GameStartedByHost
              gameLoop server client game
            else do
              sendError clientHandle "Players not ready."
              gameLoop server client game
        else do
          sendError clientHandle "Only the host can start the game."
          gameLoop server client game
    GameInfo -> do
      gameLis <- readTVarIO games
      sendEncoded clientHandle $ GameInfoAnswer (gameLis!game)
      gameLoop server client game
    GameClosedByHost -> do
      removeClientInGame server client
      sendMessage clientHandle "Game was closed by Host."
      mainLoop server client
    GameLeave -> do
      leaveGame server client game
      mainLoop server client
    GameStartedByHost -> do
      sendMessage clientHandle "Game started..."
      inGameLoopPar server client game
    PlayerConfig{..} -> do
      gameLis <- readTVarIO games
      atomically $ writeTVar games
        $ Map.adjust (updatePlayer clientName playerCiv playerTeam playerReady) game gameLis
      gameLoop server client game
    Logout ->
      sendMessage clientHandle "You have been logged out."
    _ -> do
      sendError clientHandle "Unknown Message."
      gameLoop server client game

-- |Loop for Host in running Game
inGameLoopHost :: Server -> Client -> GameName -> IO ()
inGameLoopHost server@Server{..} client@Client{..} game = do
  msg <- atomically $ readTChan clientChan
  case msg of
    GameResultMessage{..} -> do
      broadcastGame server game $ Broadcast "Game Over."
      leaveGame server client game
      mainLoop server client
    Broadcast content -> do
      sendMessage clientHandle content
      mainLoop server client
    Logout ->
      sendMessage clientHandle "You have been logged out."
    _ -> do
      sendError clientHandle "Unknown Message."
      inGameLoopHost server client game

-- |Loop for normal participant in running Game
inGameLoopPar :: Server -> Client -> GameName -> IO ()
inGameLoopPar server@Server{..} client@Client{..} game = do
  msg <- atomically $ readTChan clientChan
  case msg of
    GameClosedByHost -> do
      removeClientInGame server client
      sendMessage clientHandle "Game was closed by Host."
      mainLoop server client
    Broadcast content -> do
      sendMessage clientHandle content
      mainLoop server client
    _ -> do
      sendError clientHandle "Unknown Message."
      inGameLoopPar server client game
