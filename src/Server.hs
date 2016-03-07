{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-|
 -Copyright 2016-2016 the openage authors.
 -See copying.md for legal info.
 -}
module Server where

import Database.Persist
import Data.ByteString.Lazy as BL
import Data.ByteString as B
import Data.Aeson
import Data.Map.Strict as Map
import System.IO as S
import Control.Concurrent
import Text.Printf
import Control.Monad
import Control.Concurrent.STM
import Data.Version
import Data.Maybe
import Network
import Config
import Protocol
import DBSchema

myVersion = Version [1, 0, 0] []

-- | Global server state
data Server = Server {
  games :: TVar (Map GameName Game),
  clients :: TVar (Map AuthPlayerName Client)
  }

newServer :: IO Server
newServer = do
  games <- newTVarIO Map.empty
  clients <- newTVarIO Map.empty
  return Server{..}

-- | Server state helper functions
getGameList :: Server -> IO [Game]
getGameList server@Server{..} = atomically $ do
  gameList <- readTVar games
  return $ Map.elems gameList

checkAddGame :: Server -> ClientMessage -> IO (Maybe Game)
checkAddGame server@Server{..} (GameInit gName gMap gPlay) =
  atomically $ do
    gameMap <- readTVar games
    if Map.member gName gameMap
      then return Nothing
      else do game <- newGame gName gMap gPlay
              writeTVar games $ Map.insert gName game gameMap
              return $ Just game
checkAddGame _ _ = return Nothing

startServer :: IO()
startServer = withSocketsDo $ do
  (conf, _) <- loadConf
  port <- getPort conf
  server <- newServer
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  forever $ do
      (handle, host, clientPort) <- accept sock
      printf "Accepted connection from %s: %s\n" host (show clientPort)
      forkFinally (talk handle server)
        (\_ -> printf "Connection from %s closed\n" host >>
               sendError handle "Unknown Message." >> hClose handle)

talk :: S.Handle -> Server -> IO()
talk handle server = do
  S.hSetNewlineMode handle universalNewlineMode
  S.hSetBuffering handle LineBuffering
  getVersion handle
  mayClient <- checkAddClient handle
  case mayClient of
    Just client -> do
      sendMessage handle "Login success."
      mainLoop server client
    Nothing -> sendError handle "Login failed."

-- | Compare Version to own
getVersion :: S.Handle -> IO ()
getVersion handle = do
  verJson <- B.hGetLine handle
  if (peerProtocolVersion .
      fromJust .
      decode .
      BL.fromStrict) verJson == myVersion
    then
      sendMessage handle "Version accepted."
    else do
      sendError handle "Incompatible Version."
      thread <- myThreadId
      killThread thread

-- | Get login credentials from handle and checkLogin
checkAddClient :: S.Handle -> IO (Maybe Client)
checkAddClient handle = do
  loginJson <- B.hGetLine handle
  let Just loginDecoded = (decode . BL.fromStrict) loginJson
  checkPassw handle loginDecoded

checkPassw :: Handle -> ClientMessage -> IO(Maybe Client)
checkPassw handle Login{..} = do
  Just (Entity _ Player{..}) <- getPlayer loginName
  if loginPassword == playerPassword
    then return $ Just Client{clientName=playerUsername,
                              clientHandle=handle}
    else return Nothing
checkPassw _ _ = return Nothing

-- | Main Lobby loop with ClientMessage Handler functions
mainLoop :: Server -> Client-> IO ()
mainLoop server@Server{..} client@Client{..} = do
  received <- B.hGetLine clientHandle
  let Just mess = (decode . BL.fromStrict) received
  case mess of
    GameQuery -> do
      gameList <- getGameList server
      sendGameQueryAnswer clientHandle gameList
      mainLoop server client
    GameInit{..} -> do
      maybeGame <- checkAddGame server mess
      case maybeGame of
        Just _ -> sendMessage clientHandle "Added game."
        Nothing -> sendError clientHandle "Failed adding game."
      mainLoop server client
    GameJoin{..} -> do
      gameLis <- readTVarIO games
      if member gameId gameLis
        then  do
          atomically $ writeTVar games
            $ Map.adjust (joinPlayer clientName) gameId gameLis
          sendMessage clientHandle "Joined Game."
          mainLoop server client
        else do
          sendError clientHandle "Game does not exist."
          mainLoop server client
            where
              joinPlayer name Game{..} =
                Game{gamePlayers = name:gamePlayers,
                     gameName = gameName,
                     gameMap = gameMap,
                     numPlayers = numPlayers,
                     gameState = gameState}
    _ -> sendError clientHandle "Wrong Message Format."
