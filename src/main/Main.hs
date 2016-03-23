{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Control.Monad.Reader
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
import Masterserver.Args

main :: IO ()
main = withSocketsDo $ do
  conf <- parseOpts
  let port = serverPort conf
  server <- newServer
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  forever $ do
      (handle, host, clientPort) <- accept sock
      printf "Accepted connection from %s: %s\n" host (show clientPort)
      forkFinally (runReaderT (talk handle server host) conf) (\_ ->
        printf "Connection from %s closed\n" host >> hClose handle)

talk :: (MonadReader Config m, MonadIO m)
     => Handle
     -> Server
     -> HostName
     -> m ()
talk handle server hostname = do
  liftIO $ S.hSetNewlineMode handle universalNewlineMode
  liftIO $ S.hSetBuffering handle LineBuffering
  checkVersion handle
  mayClient <- checkAddClient handle server hostname
  case mayClient of
    Just client@Client{..} -> liftIO $ do
      sendMessage handle "Login success."
      runClient server client
        `finally` removeClientLeave server clientName
    Nothing ->
      liftIO $ sendError handle "Login failed."

-- | Compare Version to the one speciefied in the environment.
checkVersion :: (MonadReader Config m, MonadIO m)
             => S.Handle
             -> m ()
checkVersion handle = do
  verJson <- liftIO $ B.hGetLine handle
  myVersion <- asks acceptedVersion
  if ( peerProtocolVersion
     . fromJust
     . decode
     . BL.fromStrict) verJson == makeVersion myVersion
    then
      liftIO $ sendMessage handle "Version accepted."
    else liftIO $ do
      sendError handle "Incompatible Version."
      thread <- myThreadId
      killThread thread

-- | Get login credentials from handle, add client to servers
-- clientmap and return Client
checkAddClient :: (MonadReader Config m, MonadIO m)
               => Handle
               -> Server
               -> HostName
               -> m (Maybe Client)
checkAddClient handle server@Server{..} hostname = do
  loginJson <- liftIO $ B.hGetLine handle
  case (decode . BL.fromStrict) loginJson of
    Just Login{..} -> do
      let toBs = BC.pack . T.unpack
      Just (Entity _ Player{..}) <- getPlayer loginName
      if validatePassword playerPassword (toBs loginPassword)
        then do
          clientMap <- liftIO $ readTVarIO clients
          client <- liftIO $ newClient playerUsername hostname handle
          if member playerUsername clientMap
            then do
              liftIO $ sendChannel (clientMap!playerUsername) Logout
              liftIO $ atomically $ addClient server client
              return $ Just client
            else do
              liftIO $ atomically $ addClient server client
              return $ Just client
        else return Nothing
    Just AddPlayer{..} -> do
      hash <- liftIO $ hashPw pw
      res <- addPlayer name hash
      maybe (liftIO (sendError handle "Name taken.")
             >> checkAddClient handle server hostname)
        (\_ -> liftIO (sendMessage handle "Player successfully added.")
               >> checkAddClient handle server hostname) res
    _ -> do
      liftIO $ sendError handle "Unknown Format."
      return Nothing

-- | Uses BCrypt to hash pw before writing it to db
hashPw :: Text             -- ^ Password sent by client
       -> IO BC.ByteString -- ^ salted password hash
hashPw pw = do
  let toBs = BC.pack . T.unpack
  mayHash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy $ toBs pw
  maybe (error "Hashing failed") return mayHash

-- | Runs individual Client
runClient :: Server -> Client -> IO ()
runClient server@Server{..} client@Client{..} = do
  _ <- race internalReceive $ mainLoop server client
  return ()
    where
      internalReceive = forever $ do
        msg <- B.hGetLine clientHandle
        maybe (sendError clientHandle "Could not read message.")
          (sendChannel client) $ (decode . BL.fromStrict) msg

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
      -- | check if name not taken, return game if successful
      maybeGame <- atomically $ checkAddGame server clientName msg
      maybe
        -- | send Error and return to mainLoop if failed
        (sendError clientHandle "Failed adding game."
             >> mainLoop server client)
        -- | Add game to client, client to game and go to gameLoop
        (\ Game{..} ->
          atomically (joinGame server clientName gameName True)
          >> sendMessage clientHandle "Added game."
          >> gameLoop server client gameInitName) maybeGame
    GameJoin{..} -> do
      gameLis <- readTVarIO games
      case member gameId gameLis of
        True
          | Game{..} <- gameLis!gameId
          , Map.size gamePlayers < numPlayers -> do
              atomically $ joinGame server clientName gameId False
              sendMessage clientHandle "Joined Game."
              gameLoop server client gameName
          | otherwise -> do
              sendError clientHandle "Game is full."
              mainLoop server client
        _ -> do
          sendError clientHandle "Game does not exist."
          mainLoop server client
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
      atomically $ leaveGame server clientName game
      sendMessage clientHandle "Game was closed by Host."
      mainLoop server client
    GameLeave -> do
      gameLeaveHandler server client game
      gameLoop server client game
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
      atomically $ leaveGame server clientName game
      sendMessage clientHandle "Game was closed by Host."
      mainLoop server client
    GameLeave -> do
      gameLeaveHandler server client game
      gameLoop server client game
    GameOver
      | isHost -> do
          broadcastGame server game $ Broadcast "Game Over."
          gameLeaveHandler server client game
          inGameLoop server client game
      | otherwise -> do
          sendError clientHandle "Unknown Message."
          inGameLoop server client game
    Logout ->
      sendMessage clientHandle "You have been logged out."
    _ -> do
      sendError clientHandle "Unknown Message."
      inGameLoop server client game

