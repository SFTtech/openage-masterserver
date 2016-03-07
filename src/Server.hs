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
import Data.ByteString.Char8 as BC
import Data.Aeson
import Data.Text as T
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
  games :: TVar (Map GameName Game)
  }

newServer :: IO Server
newServer = do
  games <- newTVarIO Map.empty
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
      (handle, host, port) <- accept sock
      printf "Accepted connection from %s: %s\n" host (show port)
      forkFinally (talk handle server) (\_ ->
                                  printf "Connection from %s closed\n" host >> hClose handle)

talk :: S.Handle -> Server -> IO()
talk handle server = do
  S.hSetNewlineMode handle universalNewlineMode
  S.hSetBuffering handle LineBuffering
  getVersion handle
  getLogin handle
  mainLoop handle server

-- | Compare Version to own
getVersion :: S.Handle -> IO ()
getVersion handle = do
  verJson <- B.hGetLine handle
  if (decode . BL.fromStrict) verJson == Just myVersion
    then
      sendMessage handle "Version accepted."
    else do
      sendError handle "Incompatible Version."
      thread <- myThreadId
      killThread thread

-- | Get login credentials from handle and checkLogin
getLogin :: S.Handle -> IO ()
getLogin handle = do
  loginJson <- B.hGetLine handle
  correct <- (checkLogin . fromJust . decode . BL.fromStrict) loginJson
  if correct
    then
      sendMessage handle "Login success."
    else do
      sendError handle "Login failed."
      thread <- myThreadId
      killThread thread

-- | Authenticate Credentials
checkLogin :: LoginMessage -> IO Bool
checkLogin login = do
  Just (Entity _ player) <- getPlayer $ loginName login
  return $ loginPassword login == playerPassword player

-- | Main Lobby loop with ClientMessage Handler functions
mainLoop :: S.Handle -> Server -> IO ()
mainLoop handle server@Server{..} = do
  received <- B.hGetLine handle
  let Just mess = (decode . BL.fromStrict) received
  case mess of
    GameQuery -> do
      gameList <- getGameList server
      sendGameQueryAnswer handle gameList
      mainLoop handle server
    GameInit{..} -> do
      maybeGame <- checkAddGame server mess
      case maybeGame of
        Just game -> sendMessage handle "Added Game."
        Nothing -> sendError handle "Couldn't add game."
      mainLoop handle server
    _ -> sendError handle "Wrong Message Format."


-- | Game Server send Functions
sendGameQueryAnswer :: Handle -> [Game] -> IO ()
sendGameQueryAnswer handle list =
  (BC.hPutStrLn handle . BL.toStrict . encode) $ GameQueryAnswer list

sendMessage :: Handle -> Text -> IO()
sendMessage handle text =
  (BC.hPutStrLn handle . BL.toStrict . encode) $ Message text

sendError :: Handle -> Text -> IO()
sendError handle text =
  (BC.hPutStrLn handle . BL.toStrict . encode) $ Protocol.Error text
