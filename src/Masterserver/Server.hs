{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Better performance for strict records
{-# OPTIONS_GHC -funbox-strict-fields #-}

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
import Control.Monad
import Data.Aeson
import Data.ByteString.Char8 as BC
import Data.ByteString.Lazy as BL
import Data.List as L
import Data.Map.Strict as Map
import Data.Text
import Network
import System.IO as S

import Masterserver.Protocol as P

-- | Server Datatype
-- Stores Map of running games and Map of logged in clients.
data Server = Server
  { -- | Map of open games on the server
    games :: TVar (Map GameName Game)
    -- | Map of connected clients
  , clients :: TVar (Map AuthPlayerName Client)
  }

-- | Returns a Server with empty games and clients Maps.
newServer :: IO Server
newServer = do
  games <- newTVarIO Map.empty
  clients <- newTVarIO Map.empty
  return Server{..}

-- | Client datatype
-- It stores players name, handle and a channel to address it.
data Client = Client {
  clientName :: !AuthPlayerName,  -- ^ Name of logged in account
  clientAddr :: !HostName,        -- ^ Clients ip
  clientHandle :: !Handle,        -- ^ Clients Socket
  clientChan :: TChan InMessage, -- ^ Channel client listens on
  clientInGame :: Maybe Text     -- ^ Game client has joined
  }

-- |Client constructor
newClient :: AuthPlayerName -> HostName -> Handle -> IO Client
newClient clientName clientAddr clientHandle = do
  clientChan <- newTChanIO
  return Client{clientInGame=Nothing,..}

-- | Sends InMessage to the clients channel
sendChannel :: Client -> InMessage -> IO ()
sendChannel Client{..} msg =
  atomically $ writeTChan clientChan msg

-- | Send OutMessage over handle to client
sendEncoded :: ToJSON a => Handle -> a -> IO()
sendEncoded handle = BC.hPutStrLn handle . BL.toStrict . encode

-- | Send encoded GameQueryAnswer
sendGameQueryAnswer :: Handle -> [Game] -> IO ()
sendGameQueryAnswer handle list =
  sendEncoded handle $ GameQueryAnswer list

-- | Send encoded Message
sendMessage :: Handle -> Text -> IO()
sendMessage handle text =
  sendEncoded handle $ Message text

-- | Send encoded Error message
sendError :: Handle -> Text -> IO()
sendError handle text =
  sendEncoded handle $ P.Error text

-- | Get List of Games in servers game map
getGameList :: Server -> STM [Game]
getGameList Server{..} = do
  gameList <- readTVar games
  return $ Map.elems gameList

-- | Add game to servers game map
checkAddGame :: Server           -- ^ Global server containing Maps
             -> AuthPlayerName   -- ^ This Clients name
             -> InMessage        -- ^ GameInit message
             -> STM (Maybe Game) -- ^ resulting Game if name is not taken
checkAddGame Server{..} pName GameInit{..} = do
    gamesMap <- readTVar games
    if Map.member gameInitName gamesMap
        then return Nothing
        else do
          game <- newGame gameInitName pName gameInitMap maxPlayers
          writeTVar games $ Map.insert gameInitName game gamesMap
          return $ Just game
checkAddGame _ _ _ = return Nothing

-- | Remove Game from servers game map
removeGame :: Server -> GameName -> STM ()
removeGame Server{..} game =
  modifyTVar' games $ Map.delete game

-- | Add client to servers clients map
addClient :: Server -> Client -> STM ()
addClient Server{..} client@Client{..} = do
  clientsMap <- readTVar clients
  if member clientName clientsMap
    then retry
    else writeTVar clients $ Map.insert clientName client clientsMap

-- | Removes client from servers clientmap and leaves game
removeClientLeave :: Server -> AuthPlayerName -> IO ()
removeClientLeave server@Server{..} cliName = do
  clientsMap <- readTVarIO clients
  let client@Client{..} = clientsMap!cliName
  maybe (return ()) (gameLeaveHandler server client) clientInGame
  atomically $ modifyTVar' clients $ Map.delete clientName

-- | Add game to Host and Participant to games map of players
joinGame :: Server         -- ^ Global server containing Maps
         -> AuthPlayerName -- ^ This Clients name
         -> GameName       -- ^ Name of game to join
         -> Bool           -- ^ True if ready
         -> STM ()
joinGame Server{..} clientName gameName rdy= do
  gamesMap <- readTVar games
  clientsMap <- readTVar clients
  writeTVar clients
    $ Map.adjust (addClientInGame gameName) clientName clientsMap
  writeTVar games
    $ Map.adjust (addParticipant clientName
                  rdy) gameName gamesMap

-- | Delete clientName from game and gameName from client in Maps
leaveGame :: Server         -- ^ Global server containing Maps
          -> AuthPlayerName -- ^ This Clients name
          -> GameName       -- ^ Name of game to leave
          -> STM ()
leaveGame Server{..} clientName game = do
  let leavePlayer gameOld@Game{..} =
        gameOld {gamePlayers = Map.delete clientName gamePlayers}
  clientsMap <- readTVar clients
  gamesMap <- readTVar games
  writeTVar clients $ Map.adjust removeClientInGame clientName clientsMap
  when (member game gamesMap) $
    writeTVar games $ Map.adjust leavePlayer game gamesMap

-- | Leave Game if normal player, close if host
gameLeaveHandler :: Server -> Client -> GameName -> IO()
gameLeaveHandler server@Server{..} Client{..} game = do
      gameLis <- readTVarIO games
      if clientName == gameHost (gameLis!game)
        then do
          broadcastGame server game GameClosedByHost
          atomically $ removeGame server game
        else do
          atomically $ leaveGame server clientName game
          sendMessage clientHandle "Left Game."

-- | Add Game to Clients clientInGame field
addClientInGame :: GameName -> Client -> Client
addClientInGame game client@Client{..} =
  client {clientInGame = Just game}

-- | Remove Game from clientsInGame field
removeClientInGame :: Client -> Client
removeClientInGame cli = cli {clientInGame = Nothing}

-- | Add participant to game
addParticipant :: AuthPlayerName -- ^ Players name
           -> Bool               -- ^ True if player is host
           -> Game               -- ^ Game to update
           -> Game               -- ^ Resulting game
addParticipant name host game@Game{..} =
  game {gamePlayers = Map.insert name
        (newParticipant name host) gamePlayers}

-- | Update game with given map, mode and player number
updateGame :: Text -- ^ Games map name
           -> Text -- ^ Game mode
           -> Int  -- ^ Maximal number of Players
           -> Game -- ^ Game to update
           -> Game -- ^ Resulting game
updateGame gMap mode num game =
  game {gameMap=gMap, gameMode=mode, numPlayers=num}

-- | Updates player configuration
updatePlayer :: AuthPlayerName -- ^ Players name
             -> Text           -- ^ Players civilization
             -> Int            -- ^ Team number
             -> Bool           -- ^ True if player is ready
             -> Game           -- ^ Game to update
             -> Game           -- ^ Resulting game
updatePlayer name civ team rdy game@Game{..} =
  game {gamePlayers = Map.adjust updateP name gamePlayers}
    where
      updateP par = par { parName = name
                        , parCiv = civ
                        , parTeam=team
                        , parReady=rdy}

-- | Broadcast message to all Clients in a Game
broadcastGame :: Server -> GameName -> InMessage -> IO ()
broadcastGame Server{..} gameName msg = do
  clientLis <- readTVarIO clients
  gameLis <- readTVarIO games
  mapM_ (flip sendChannel msg . (!) clientLis . parName)
    $ gamePlayers $ gameLis!gameName

-- | Convert the clientmap with filter to the map format used in
-- GameStartAnswer
convMap :: Map.Map AuthPlayerName Client -- ^ Servers clients map
        -> [AuthPlayerName]              -- ^ List of clients to convert
        -> Map.Map AuthPlayerName HostName -- ^ Resulting GameStart map
convMap inMap lis =
  Map.map clientAddr (Map.filterWithKey (\k _ -> k `L.elem` lis) inMap)
