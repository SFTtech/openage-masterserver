{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
 -Copyright 2016-2016 the openage authors.
 -See copying.md for legal info.
 -}
module Protocol where

import Data.Map.Strict as Map
import Data.Aeson
import Data.Aeson.TH
import Data.Version
import Data.Text(Text)
import System.IO
import Control.Concurrent.STM
import Data.ByteString.Lazy as BL
import Data.ByteString.Char8 as BC

-- |Client datatype
-- It stores players name, handle and a channel to address it.
data Client = Client {
  clientName :: AuthPlayerName,
  clientHandle :: Handle,
  clientChan :: TChan InMessage,
  clientInGame :: Maybe Text
  }

instance Show Client where
  show Client{..} = show clientName ++ show clientInGame

type AuthPlayerName = Text

-- |Client constructor
newClient :: AuthPlayerName -> Handle -> IO Client
newClient clientName clientHandle = do
  clientChan <- newTChanIO
  return Client{clientInGame=Nothing,..}

-- |Sends InMessage to the clients channel
sendChanMessage :: Client -> InMessage -> STM ()
sendChanMessage Client{..} = writeTChan clientChan

-- |Game datatype
-- It stores information about an open game
data Game = Game {
  gameHost :: AuthPlayerName,
  gameName:: GameName,
  gameMap :: Text,
  numPlayers :: Int,
  gamePlayers :: Map AuthPlayerName Participant,
  gameState :: GameStat
  } deriving Show

type GameName = Text

-- | Game Status
data GameStat = Lobby | Running | Aborted | Finished
  deriving (Show, Read, Eq)

newGame :: GameName -> AuthPlayerName -> Text -> Int -> STM Game
newGame gameName gameHost gameMap numPlayers =
  return Game {gameState=Lobby, gamePlayers=Map.empty, ..}

data Participant = Participant {
  parName :: AuthPlayerName,
  parCiv :: Text,
  parTeam :: Int,
  parReady :: Bool
  } deriving Show

newParticipant :: AuthPlayerName -> Bool -> Participant
newParticipant parName parReady = Participant{parCiv="Britain",
                                              parTeam=0, ..}

-- | Messages sent by Client
data InMessage =
  Broadcast {msg :: Text} |
  Login {
    loginName :: AuthPlayerName,
    loginPassword :: Text
  } |
  GameClosedByHost |
  GameInfo |
  GameInit {
    gameInitName :: GameName,
    gameInitMap :: Text,
    maxPlayers :: Int
  } |
  GameJoin {
    gameId :: GameName
  } |
  GameLeave |
  GameQuery |
  GameResultMessage {result :: GameResult} |
  GameStart |
  GameStartedByHost |
  Logout |
  PlayerConfig {
    playerCiv :: Text,
    playerTeam :: Int,
    playerReady :: Bool
  } |
  VersionMessage {
    peerProtocolVersion :: Version
  } deriving (Show, Read, Eq)

data GameResult = Victory | Defeat
  deriving (Show, Read, Eq)

-- | Messages sent by Server
data OutMessage =
  GameQueryAnswer {gameList :: [Game]} |
  GameInfoAnswer {game :: Game} |
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

sendChannel :: Client -> InMessage -> IO ()
sendChannel Client{..} msg =
  atomically $ writeTChan clientChan msg

Prelude.concat <$> mapM (deriveJSON defaultOptions) [''InMessage,
                                                     ''Participant,
                                                     ''Game,
                                                     ''GameStat,
                                                     ''GameResult,
                                                     ''OutMessage,
                                                     ''Version]
