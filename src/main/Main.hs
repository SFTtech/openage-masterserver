{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Copyright 2016-2016 the openage authors. See copying.md for legal info.
-- Module: Main
--
-- Main entry file for the openage masterserver
-- this server will listen on a tcp socket
-- and provide a funny API for gameservers and clients
-- to start communicating with each other.

-----------------------------------------------------------------------------
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception.Base (finally)
import Control.Monad
import Crypto.BCrypt
import Data.Aeson
import Data.ByteString as B
import Data.ByteString.Lazy as BL
import Data.ByteString.Char8 as BC
import Data.List as L
import Data.Map.Strict as Map
import Data.Maybe
import Data.Text as T
import Data.Version (makeVersion)
import Database.Persist
import Network
import Text.Printf
import System.IO as S

import Masterserver.Config
import Masterserver.Database
import Masterserver.Protocol as P
import Masterserver.Server

main :: IO ()
main = withSocketsDo $ do
  port <- getPort
  server <- newServer
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  forever $ do
      (handle, host, clientPort) <- accept sock
      printf "Accepted connection from %s: %s\n" host (show clientPort)
      forkFinally (talk handle server host) (\_ ->
        printf "Connection from %s closed\n" host >> hClose handle)

talk :: Handle -> Server -> HostName -> IO()
talk handle server hostname = do
  S.hSetNewlineMode handle universalNewlineMode
  S.hSetBuffering handle LineBuffering
  checkVersion handle
  mayClient <- checkAddClient handle server hostname
  case mayClient of
    Just client@Client{..} -> do
      sendMessage handle "Login success."
      runClient server client `finally` removeClient server clientName
    Nothing ->
      sendError handle "Login failed."

-- | Compare Version to own
checkVersion :: S.Handle -> IO ()
checkVersion handle = do
  verJson <- B.hGetLine handle
  myVersion <- getVersion
  if ( peerProtocolVersion
     . fromJust
     . decode
     . BL.fromStrict) verJson == makeVersion myVersion
    then
      sendMessage handle "Version accepted."
    else do
      sendError handle "Incompatible Version."
      thread <- myThreadId
      killThread thread

-- | Get login credentials from handle, add client to servers
-- clientmap and return Client
checkAddClient :: Handle -> Server -> HostName -> IO(Maybe Client)
checkAddClient handle server@Server{..} hostname = do
  loginJson <- B.hGetLine handle
  case (decode . BL.fromStrict) loginJson of
    Just Login{..} -> do
      Just (Entity _ Player{..}) <- getPlayer loginName
      if validatePassword playerPassword (toBs loginPassword)
        then do
          -- | TODO: atomical checkAdd function
          clientMap <- readTVarIO clients
          client <- newClient playerUsername hostname handle
          if member playerUsername clientMap
            then do
              sendChannel (clientMap!playerUsername) Logout
              atomically $ do
                clientsMap <- readTVar clients
                writeTVar clients
                  $ Map.insert playerUsername client clientsMap
              return $ Just client
            else atomically $ do
              writeTVar clients
                $ Map.insert playerUsername client clientMap
              return $ Just client
        else return Nothing
        where
          toBs = BC.pack . T.unpack
    Just AddPlayer{..} -> do
      hash <- hashPw pw
      res <- addPlayer name hash
      case res of
        Just _ -> do
          sendMessage handle "Player successfully added."
          checkAddClient handle server hostname
        Nothing -> do
          sendError handle "Name taken."
          checkAddClient handle server hostname
    _ -> do
      sendError handle "Unknown Format."
      return Nothing

-- | Uses BCrypt to hash pw before writing it to db
hashPw :: Text             -- ^ Password sent by client
       -> IO BC.ByteString -- ^ salted password hash
hashPw pw = do
  let toBs = BC.pack . T.unpack
  mayHash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy $ toBs pw
  case mayHash of
    Just hash -> return hash
    Nothing -> error "Could not hash."

-- | Runs individual Client
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

-- | Remove Client from servers client map and close his games
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

-- | Main Lobby loop with ClientMessage Handler functions
mainLoop :: Server -> Client -> IO ()
mainLoop server@Server{..} client@Client{..} = do
  msg <- atomically $ readTChan clientChan
  case msg of
    GameQuery -> do
      gameLis <- atomically $ getGameList server
      sendGameQueryAnswer clientHandle gameLis
      mainLoop server client
    GameInit{..} -> do
      maybeGame <- atomically $ checkAddGame server clientName msg
      case maybeGame of
        Just Game{..} -> do
          gameLis <- readTVarIO games
          clientLis <- readTVarIO clients
          atomically $ writeTVar clients
            $ Map.adjust (addClientInGame gameName) clientName clientLis
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

-- | Gamestate loop
gameLoop :: Server -> Client -> GameName -> IO ()
gameLoop server@Server{..} client@Client{..} game= do
  msg <- atomically $ readTChan clientChan
  gameLis <- readTVarIO games
  let isHost = clientName == gameHost (gameLis!game)
      thisPlayers = gamePlayers $ gameLis!game
  case msg of
    ChatFromClient{..} -> do
      broadcastGame server game
        $ ChatFromThread clientName chatFromCContent
      gameLoop server client game
    ChatFromThread{..} -> do
      sendEncoded clientHandle
        $ ChatOut chatFromTOrign chatFromTContent
      gameLoop server client game
    GameStart
      | isHost && L.all parReady thisPlayers -> do
          clientLis <- readTVarIO clients
          broadcastGame server game GameStartedByHost
          sendEncoded clientHandle
            $ GameStartAnswer $ convMap clientLis (keys thisPlayers)
          gameLoop server client game
      | isHost -> do
          sendError clientHandle "Players not ready."
          gameLoop server client game
      | otherwise -> do
          sendError clientHandle "Only the host can start the game."
          gameLoop server client game
    GameInfo -> do
      sendEncoded clientHandle $ GameInfoAnswer (gameLis!game)
      gameLoop server client game
    GameConfig{..}
      | isHost &&
        gameConfPlayerNum >= (Map.size . gamePlayers) (gameLis!game)-> do
          atomically $ do
            gamesMap <- readTVar games
            writeTVar games
              $ Map.adjust (updateGame gameConfMap gameConfMode
                            gameConfPlayerNum) game gamesMap
          gameLoop server client game
      | isHost -> do
          sendError clientHandle "Can't choose less Players."
          gameLoop server client game
      | otherwise -> do
          sendError clientHandle "Unknown Message."
          inGameLoop server client game
    GameClosedByHost -> do
      removeClientInGame server client
      sendMessage clientHandle "Game was closed by Host."
      mainLoop server client
    GameLeave -> do
      leaveGame server client game
      mainLoop server client
    GameStartedByHost -> do
      sendMessage clientHandle "Game started..."
      inGameLoop server client game
    PlayerConfig{..} -> do
      atomically $ do
        gamesMap <- readTVar games
        writeTVar games
          $ Map.adjust (updatePlayer clientName playerCiv playerTeam
                        playerReady) game gamesMap
      gameLoop server client game
    Logout ->
      sendMessage clientHandle "You have been logged out."
    _ -> do
      sendError clientHandle "Unknown Message."
      gameLoop server client game

-- | Loop for Host in running Game
inGameLoop :: Server -> Client -> GameName -> IO ()
inGameLoop server@Server{..} client@Client{..} game = do
  msg <- atomically $ readTChan clientChan
  gameLis <- readTVarIO games
  let isHost = clientName == gameHost (gameLis!game)
  case msg of
    Broadcast{..} -> do
      sendMessage clientHandle content
      inGameLoop server client game
    ChatFromClient{..} -> do
      broadcastGame server game
        $ ChatFromThread clientName chatFromCContent
      inGameLoop server client game
    ChatFromThread{..} -> do
      sendEncoded clientHandle
        $ ChatOut chatFromTOrign chatFromTContent
      inGameLoop server client game
    GameClosedByHost -> do
      removeClientInGame server client
      sendMessage clientHandle "Game was closed by Host."
      mainLoop server client
    GameLeave -> do
      leaveGame server client game
      gameLoop server client game
    GameOver
      | isHost -> do
          broadcastGame server game $ Broadcast "Game Over."
          leaveGame server client game
          inGameLoop server client game
      | otherwise -> do
          sendError clientHandle "Unknown Message."
          inGameLoop server client game
    Logout ->
      sendMessage clientHandle "You have been logged out."
    _ -> do
      sendError clientHandle "Unknown Message."
      inGameLoop server client game

-- | Join Game and return True if join was successful
-- TODO: remove nested if-clauses
joinGame :: Server -> Client -> GameName -> IO Bool
joinGame Server{..} Client{..} gameId = do
  gameLis <- readTVarIO games
  if member gameId gameLis
    then do
      let Game{..} = gameLis!gameId
      if Map.size gamePlayers < numPlayers
        then do
          atomically $ do
            gamesMap <- readTVar games
            clientsMap <- readTVar clients
            writeTVar clients
              $ Map.adjust (addClientInGame gameId) clientName clientsMap
            writeTVar games
              $ Map.adjust (joinPlayer clientName False) gameId gamesMap
          sendMessage clientHandle "Joined Game."
          return True
        else do
          sendError clientHandle "Game is full."
          return False
    else do
      sendError clientHandle "Game does not exist."
      return False

-- | Leave Game if normal player, close if host
leaveGame :: Server -> Client -> GameName -> IO()
leaveGame server@Server{..} client@Client{..} game = do
      gameLis <- readTVarIO games
      if clientName == gameHost (gameLis!game)
        then do
          clientLis <- readTVarIO clients
          mapM_ (flip sendChannel GameClosedByHost
                 . (!) clientLis. parName)
            $ gamePlayers $ gameLis!game
          atomically $ removeGame server game
        else do
          removeClientInGame server client
          atomically $ do
            gamesMap <- readTVar games
            writeTVar games
              $ Map.adjust leavePlayer game gamesMap
          sendMessage clientHandle "Left Game."
            where
              leavePlayer gameOld@Game{..} =
                gameOld {gamePlayers = Map.delete clientName gamePlayers}
