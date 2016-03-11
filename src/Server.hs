{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-|
 -Copyright 2016-2016 the openage authors.
 -See copying.md for legal info.
 -}
module Server where

import Control.Exception.Base
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

-- |Remove Game from servers game map
removeGame :: Server -> Text -> IO ()
removeGame Server{..} name = atomically $
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
      forkFinally (talk handle server) (\_ ->
        printf "Connection from %s closed\n" host >> hClose handle)

talk :: S.Handle -> Server -> IO()
talk handle server = do
  S.hSetNewlineMode handle universalNewlineMode
  S.hSetBuffering handle LineBuffering
  getVersion handle
  mayClient <- checkAddClient server handle
  case mayClient of
    Just client@Client{..} -> do
      sendMessage handle "Login success."
      runClient server client `finally` removeClient server clientName
    Nothing ->
      sendError handle "Login failed."

removeClient :: Server -> Text -> IO ()
removeClient server@Server{..} clientName = do
  clientLis <- readTVarIO clients
  let client = clientLis!clientName
  printf "Debug: Client: %s\n" $ show client
  case clientInGame client of
    Just game -> do
      leaveGame server client game
      cleanTvar
    Nothing ->
      cleanTvar
  where
    cleanTvar = atomically $
      modifyTVar' clients $ Map.delete clientName

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
checkAddClient :: Server -> Handle -> IO(Maybe Client)
checkAddClient Server{..} handle = do
  loginJson <- B.hGetLine handle
  case (decode . BL.fromStrict) loginJson of
    Just Login{..} -> do
      Just (Entity _ Player{..}) <- getPlayer loginName
      if loginPassword == playerPassword
        then atomically $ do
          clientMap <- readTVar clients
          client <- newClient playerUsername handle
          writeTVar clients
            $ Map.insert playerUsername client clientMap
          return $ Just client
        else return Nothing
    _ -> do
      sendError handle "Unknown Format."
      return Nothing

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
        Just Game{..} -> do
          clientLis <- readTVarIO clients
          atomically $ writeTVar clients
            $ Map.adjust (addClientGame gameName) clientName clientLis
          sendMessage clientHandle "Added game."
          gameLoop server client gameInitName
        Nothing -> do
          sendError clientHandle "Failed adding game."
          mainLoop server client
        where addClientGame game client@Client{..} =
                client {clientInGame = Just game}
    GameJoin{..} -> do
      gameLis <- readTVarIO games
      if member gameId gameLis
        then  do
          clientLis <- readTVarIO clients
          atomically $ writeTVar clients
            $ Map.adjust (addClientGame gameId) clientName clientLis
          atomically $ writeTVar games
            $ Map.adjust (joinPlayer clientName) gameId gameLis
          sendMessage clientHandle "Joined Game."
          gameLoop server client gameId
        else do
          sendError clientHandle "Game does not exist."
          mainLoop server client
            where
              joinPlayer name game@Game{..} =
                game {gamePlayers = name:gamePlayers}
              addClientGame game client@Client{..} =
                client {clientInGame = Just game}
    _ -> do
      sendError clientHandle "Unknown Message."
      mainLoop server client

gameLoop :: Server -> Client -> Text -> IO ()
gameLoop server@Server{..} client@Client{..} game= do
  msg <- atomically $ readTChan clientChan
  case msg of
    GameClosedByHost -> do
      sendMessage clientHandle "Game was closed by Host."
      mainLoop server client
    GameLeave -> do
      leaveGame server client game
      mainLoop server client
    _ -> do
      sendError clientHandle "Unknown Message."
      gameLoop server client game

leaveGame :: Server -> Client -> Text -> IO()
leaveGame server@Server{..} client@Client{..} game = do
      gameLis <- readTVarIO games
      if clientName == gameHost (gameLis!game)
        then do
          clientLis <- readTVarIO clients
          mapM_ (sendCliGameClosed . (!) clientLis)
            $ gamePlayers $ gameLis!game
          removeGame server game
          sendMessage clientHandle "Closed Game."
        else do
          clientLis <- readTVarIO clients
          atomically $ writeTVar clients
            $ Map.adjust rmClientGame clientName clientLis
          atomically $ writeTVar games
            $ Map.adjust leavePlayer game gameLis
          sendMessage clientHandle "Left Game."
            where
              leavePlayer gameOld@Game{..} =
                gameOld {gamePlayers = L.delete clientName gamePlayers}
              rmClientGame client@Client{..}=
                client {clientInGame = Nothing}
