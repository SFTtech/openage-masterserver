{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
 -Copyright 2016-2016 the openage authors.
 -See copying.md for legal info.
 -}
module Protocol where

import Data.Aeson
import Data.Aeson.TH
import Data.Version
import Data.Text(Text)
import System.IO
import Database.Persist.TH
import Control.Concurrent.STM
import Data.ByteString.Lazy as BL
import Data.ByteString.Char8 as BC

-- |Client datatype
-- It stores players name, handle and a channel to address it.
data Client = Client {
  clientName :: AuthPlayerName,
  clientHandle :: Handle,
  clientChan :: TChan InMessage
  }

type AuthPlayerName = Text

-- |Client constructor
newClient :: Text -> Handle -> STM Client
newClient clientName clientHandle = do
  clientChan <- newTChan
  return Client{..}

-- |Sends InMessage to the clients channel
sendChanMessage :: Client -> InMessage -> STM ()
sendChanMessage Client{..} = writeTChan clientChan

-- |Game datatype
-- It stores information about an open game
data Game = Game {
  gameHost :: AuthPlayerName,
  gameName:: Text,
  gameMap :: Text,
  numPlayers :: Int,
  gamePlayers :: [AuthPlayerName],
  gameState :: GameStat
  } deriving Show

type GameName = Text

-- | Game Status
data GameStat = Lobby | Running | Aborted | Finished
  deriving (Show, Read, Eq)

newGame :: AuthPlayerName -> Text -> Text -> Int -> STM Game
newGame gameName gameHost gameMap numPlayers =
  return Game {gameState=Lobby, gamePlayers=[], ..}

-- | Messages sent by Client
data InMessage =
  GameClosedByHost |
  Login {
    loginName :: Text,
    loginPassword :: Text
  } |
  GameInit {
    gameInitName :: Text,
    gameInitMap :: Text,
    maxPlayers :: Int
  } |
  GameJoin {
    gameId :: Text
  } |
  GameLeave |
  GameQuery |
  PlayerQuery |
  VersionMessage {
    peerProtocolVersion :: Version
  }
  deriving (Show, Read, Eq)

-- | Messages sent by Server
data ServerMessage =
  GameQueryAnswer {gameList :: [Game]} |
  Error {errorString :: Text} |
  Message {messageString :: Text}
  deriving Show

-- | Game Server send Functions
sendEncoded :: ToJSON a => Handle -> a -> IO()
sendEncoded handle = BC.hPutStrLn handle . BL.toStrict . encode

sendGameQueryAnswer :: Handle -> [Game] -> IO ()
sendGameQueryAnswer handle list =
  sendEncoded handle $ GameQueryAnswer list

sendMessage :: Handle -> Text -> IO()
sendMessage handle text =
  sendEncoded handle $ Message text

sendError :: Handle -> Text -> IO()
sendError handle text =
  sendEncoded handle $ Protocol.Error text

sendCliGameClosed :: Client -> IO ()
sendCliGameClosed Client{..} =
  atomically $ writeTChan clientChan GameClosedByHost

Prelude.concat <$> mapM (deriveJSON defaultOptions) [''InMessage,
                                             ''Game,
                                             ''GameStat,
                                             ''ServerMessage,
                                             ''Version]
