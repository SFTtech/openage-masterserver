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

-- |Remove Game from servers game map
removeGame :: Server -> GameName -> IO ()
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

-- |Compare Version to own
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

-- |Remove Client from servers client map and close his games
removeClient :: Server -> AuthPlayerName -> IO ()
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

-- |Runs individual Client
runClient :: Server -> Client -> IO ()
runClient server@Server{..} client@Client{..} = do
  _ <- race internalReceive $ mainLoop server client
  return ()
    where
      internalReceive = forever $ do
        msg <- B.hGetLine clientHandle
        case (decode . BL.fromStrict) msg of
          Just mess -> atomically $ sendChanMessage client mess
          Nothing -> sendError clientHandle "Could not read message."

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
        where addClientGame game client@Client{..} =
                client {clientInGame = Just game}
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
      clientLis <- readTVarIO clients
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
    Broadcast msg -> do
      sendMessage clientHandle msg
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
    Broadcast msg -> do
      sendMessage clientHandle msg
      mainLoop server client
    _ -> do
      sendError clientHandle "Unknown Message."
      inGameLoopPar server client game

-- |Join Game and return True if join was successful
joinGame :: Server -> Client -> GameName -> IO Bool
joinGame server@Server{..} client@Client{..} gameId = do
  gameLis <- readTVarIO games
  if member gameId gameLis
    then do
      let Game{..} = gameLis!gameId
      if Map.size gamePlayers < numPlayers
        then do
          clientLis <- readTVarIO clients
          atomically $ writeTVar clients
            $ Map.adjust (addClientGame gameId) clientName clientLis
          atomically $ writeTVar games
            $ Map.adjust (joinPlayer clientName False) gameId gameLis
          sendMessage clientHandle "Joined Game."
          return True
        else do
          sendError clientHandle "Game is full."
          return False
    else do
      sendError clientHandle "Game does not exist."
      return False

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

addClientGame :: GameName -> Client -> Client
addClientGame game client@Client{..} =
  client {clientInGame = Just game}

-- |Leave Game if normal player, close if host
leaveGame :: Server -> Client -> GameName -> IO()
leaveGame server@Server{..} client@Client{..} game = do
      gameLis <- readTVarIO games
      if clientName == gameHost (gameLis!game)
        then do
          clientLis <- readTVarIO clients
          mapM_ (flip sendChannel GameClosedByHost
                 . (!) clientLis. parName)
            $ gamePlayers $ gameLis!game
          removeGame server game
          sendMessage clientHandle "Closed Game."
        else do
          removeClientInGame server client
          clientLis <- readTVarIO clients
          atomically $ writeTVar games
            $ Map.adjust leavePlayer game gameLis
          sendMessage clientHandle "Left Game."
            where
              leavePlayer gameOld@Game{..} =
                gameOld {gamePlayers = Map.delete clientName gamePlayers}

broadcastGame :: Server -> GameName -> InMessage -> IO ()
broadcastGame Server{..} gameName msg = do
  clientLis <- readTVarIO clients
  gameLis <- readTVarIO games
  mapM_ (flip sendChannel msg . (!) clientLis . parName)
    $ gamePlayers $ gameLis!gameName

-- |Remove ClientInGame from client in servers clientmap
removeClientInGame :: Server -> Client -> IO ()
removeClientInGame server@Server{..} client@Client{..} = do
  clientLis <- readTVarIO clients
  atomically $ writeTVar clients
    $ Map.adjust rmClientGame clientName clientLis
    where
      rmClientGame client = client {clientInGame = Nothing}
