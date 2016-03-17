{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

------------------------------------------------------------------------------
-- |
-- Copyright 2016-2016 the openage authors. See copying.md for legal info.
-- Module: Masterserver.Server
--
-- This module defines the server datatype and several helperfunctions
-- for the masterservers server logic.

------------------------------------------------------------------------------

module Masterserver.Server where

import Control.Concurrent.STM
import Data.ByteString.Char8 as BC
import Data.ByteString.Lazy as BL
import Data.Aeson
import Data.List as L
import Data.Text
import Data.Map.Strict as Map
import Network
import System.IO as S

import Masterserver.Protocol as P

-- |Server Datatype
-- Stores Map of running Games and Map of logged in clients
data Server = Server {
  games :: TVar (Map GameName Game),
  clients :: TVar (Map AuthPlayerName Client)
  }

newServer :: IO Server
newServer = do
  games <- newTVarIO Map.empty
  clients <- newTVarIO Map.empty
  return Server{..}

-- |Client datatype
-- It stores players name, handle and a channel to address it.
data Client = Client {
  clientName :: AuthPlayerName,
  clientAddr :: HostName,
  clientHandle :: Handle,
  clientChan :: TChan InMessage,
  clientInGame :: Maybe Text
  }

-- |Client constructor
newClient :: AuthPlayerName -> HostName -> Handle -> IO Client
newClient clientName clientAddr clientHandle = do
  clientChan <- newTChanIO
  return Client{clientInGame=Nothing,..}

-- |Sends InMessage to the clients channel
sendChannel :: Client -> InMessage -> IO ()
sendChannel Client{..} msg =
  atomically $ writeTChan clientChan msg

-- |Send OutMessage over handle to client
sendEncoded :: ToJSON a => Handle -> a -> IO()
sendEncoded handle = BC.hPutStrLn handle . BL.toStrict . encode

-- |Send encoded GameQueryAnswer
sendGameQueryAnswer :: Handle -> [Game] -> IO ()
sendGameQueryAnswer handle list =
  sendEncoded handle $ GameQueryAnswer list

-- |Send encoded Message
sendMessage :: Handle -> Text -> IO()
sendMessage handle text =
  sendEncoded handle $ Message text

-- |Send encoded Error message
sendError :: Handle -> Text -> IO()
sendError handle text =
  sendEncoded handle $ P.Error text

-- |Get List of Games in servers game map
getGameList :: Server -> IO [Game]
getGameList Server{..} = atomically $ do
  gameList <- readTVar games
  return $ Map.elems gameList

-- |Add game to servers game map
checkAddGame :: Server -> AuthPlayerName -> InMessage -> IO (Maybe Game)
checkAddGame Server{..} pName (GameInit gName gMap gPlay) =
  atomically $ do
    gameMap <- readTVar games
    if Map.member gName gameMap
      then return Nothing
      else do game <- newGame gName pName gMap gPlay
              writeTVar games $ Map.insert gName game gameMap
              return $ Just game
checkAddGame _ _ _ = return Nothing

updateGame :: Text -> Text -> Int -> Game -> Game
updateGame gMap mode num game =
  game {gameMap=gMap, gameMode=mode, numPlayers=num}

-- |Remove Game from servers game map
removeGame :: Server -> GameName -> IO ()
removeGame Server{..} name = atomically $
  modifyTVar' games $ Map.delete name

-- |Add participant to game
joinPlayer :: AuthPlayerName -> Bool -> Game -> Game
joinPlayer name host game@Game{..} =
  game {gamePlayers = Map.insert name
        (newParticipant name host) gamePlayers}

-- |Updates player configuration
updatePlayer :: AuthPlayerName -> Text -> Int -> Bool-> Game -> Game
updatePlayer name civ team rdy game@Game{..} =
  game {gamePlayers = Map.adjust updateP name gamePlayers }
    where
      updateP par = par {parName = name,
                         parCiv = civ,
                         parTeam=team,
                         parReady=rdy}

-- |Add Game to Clients clientInGame field
addClientInGame :: GameName -> Client -> Client
addClientInGame game client@Client{..} =
  client {clientInGame = Just game}

-- |Remove ClientInGame from client in servers clientmap
removeClientInGame :: Server -> Client -> IO ()
removeClientInGame Server{..} Client{..} = do
  clientLis <- readTVarIO clients
  atomically $ writeTVar clients
    $ Map.adjust rmClientGame clientName clientLis
    where
      rmClientGame cli = cli {clientInGame = Nothing}

-- |Broadcast message to all Clients in a Game
broadcastGame :: Server -> GameName -> InMessage -> IO ()
broadcastGame Server{..} gameName msg = do
  clientLis <- readTVarIO clients
  gameLis <- readTVarIO games
  mapM_ (flip sendChannel msg . (!) clientLis . parName)
    $ gamePlayers $ gameLis!gameName

-- |Convert the clientmap with filter to the map format used in
-- GameStartAnswer
convMap :: Map.Map AuthPlayerName Client -> [AuthPlayerName] ->
           Map.Map AuthPlayerName HostName
convMap inMap lis =
  Map.map clientAddr (Map.filterWithKey (\k _ -> k `L.elem` lis) inMap)
