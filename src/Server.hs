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
import Data.Text
import Data.List as L
import Data.Map.Strict as Map
import System.IO as S
import Control.Concurrent
import Text.Printf
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.Async
import Data.Version
import Data.Maybe
import Network
import Config
import Protocol
import DBSchema

myVersion = Version [1, 0, 0] []

-- |Global server state
data Server = Server {
  games :: TVar (Map GameName Game),
  clients :: TVar (Map AuthPlayerName Client)
  }

newServer :: IO Server
newServer = do
  games <- newTVarIO Map.empty
  clients <- newTVarIO Map.empty
  return Server{..}

-- |Server state helper functions
getGameList :: Server -> IO [Game]
getGameList server@Server{..} = atomically $ do
  gameList <- readTVar games
  return $ Map.elems gameList

-- |Add game to servers game map
checkAddGame :: Server -> AuthPlayerName -> InMessage -> IO (Maybe Game)
checkAddGame server@Server{..} pName (GameInit gName gMap gPlay) =
  atomically $ do
    gameMap <- readTVar games
    if Map.member gName gameMap
      then return Nothing
      else do game <- newGame gName pName gMap gPlay
              writeTVar games $ Map.insert gName game gameMap
              return $ Just game
checkAddGame _ _ _ = return Nothing

-- |Remove Game from servers game map
removeGame :: Server -> Text -> IO ()
removeGame server@Server{..} name = atomically $
  modifyTVar' games $ Map.delete name

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
  mayClient <- checkAddClient server handle
  case mayClient of
    Just client -> do
      sendMessage handle "Login success."
      runClient server client
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
checkAddClient :: Server -> S.Handle -> IO (Maybe Client)
checkAddClient server handle = do
  loginJson <- B.hGetLine handle
  let Just loginDecoded = (decode . BL.fromStrict) loginJson
  checkPassw server handle loginDecoded

checkPassw :: Server -> Handle -> InMessage -> IO(Maybe Client)
checkPassw Server{..} handle Login{..} = do
  Just (Entity _ Player{..}) <- getPlayer loginName
  if loginPassword == playerPassword
    then atomically $ do
      clientMap <- readTVar clients
      client <- newClient playerUsername handle
      writeTVar clients
        $ Map.insert playerUsername client clientMap
      return $ Just client
    else return Nothing
checkPassw _ _ _ = return Nothing

runClient :: Server -> Client-> IO ()
runClient server@Server{..} client@Client{..} = do
  _ <- race internalReceive $ mainLoop server client
  return ()
    where
      internalReceive = forever $ do
        msg <- B.hGetLine clientHandle
        case (decode . BL.fromStrict) msg of
          Just mess -> atomically $ sendChanMessage client mess
          Nothing -> sendError clientHandle "Could not read message."

-- | Main Lobby loop with ClientMessage Handler functions
mainLoop :: Server -> Client -> IO ()
mainLoop server@Server{..} client@Client{..} = do
  msg <- atomically $ readTChan clientChan
  case msg of
    GameQuery -> do
      gameList <- getGameList server
      sendGameQueryAnswer clientHandle gameList
      mainLoop server client
    GameInit{..} -> do
      maybeGame <- checkAddGame server clientName msg
      case maybeGame of
        Just game -> do
          sendMessage clientHandle "Added game."
          gameLoop server client game
        Nothing -> do
          sendError clientHandle "Failed adding game."
          mainLoop server client
    GameJoin{..} -> do
      gameLis <- readTVarIO games
      if member gameId gameLis
        then  do
          atomically $ writeTVar games
            $ Map.adjust (joinPlayer clientName) gameId gameLis
          sendMessage clientHandle "Joined Game."
          gameLoop server client (gameLis!gameId)
        else do
          sendError clientHandle "Game does not exist."
          mainLoop server client
            where
              joinPlayer name Game{..} =
                Game{gamePlayers = name:gamePlayers,
                     gameName = gameName,
                     gameMap = gameMap,
                     numPlayers = numPlayers,
                     gameState = gameState,
                     gameHost = gameHost}
    _ -> do
      sendError clientHandle "Unknown Tag."
      mainLoop server client

gameLoop :: Server -> Client -> Game -> IO ()
gameLoop server@Server{..} client@Client{..} game@Game{..}= do
  msg <- atomically $ readTChan clientChan
  case msg of
    GameClosedByHost -> do
      sendMessage clientHandle "Game was closed by Host."
      mainLoop server client
    GameLeave -> do
      gameLis <- readTVarIO games
      if clientName == gameHost
        then do
          clientLis <- readTVarIO clients
          mapM_ (sendCliGameClosed . (!) clientLis) gamePlayers
          removeGame server gameName
          sendMessage clientHandle "Closed Game."
          mainLoop server client
        else do
          atomically $ writeTVar games
            $ Map.adjust (leavePlayer clientName) gameName gameLis
          sendMessage clientHandle "Left Game."
          mainLoop server client
        where
          leavePlayer name Game{..} =
            Game{gamePlayers = L.delete clientName gamePlayers,
                 gameName = gameName,
                 gameMap = gameMap,
                 numPlayers = numPlayers,
                 gameState = gameState,
                 gameHost = gameHost}
    _ -> do
      sendError clientHandle "Wrong Message Format."
      gameLoop server client game
