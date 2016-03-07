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

type AuthPlayerName = Text

data Client = Client {
  clientName :: AuthPlayerName,
  clientHandle :: Handle
  } deriving Show

newClient :: Text -> Handle -> STM Client
newClient clientName clientHandle = return Client{..}

type GameName = Text

data Game = Game {
  gameName:: Text,
  gameMap :: Text,
  numPlayers :: Int,
  gamePlayers :: [AuthPlayerName],
  gameState :: GameStat
  } deriving Show

-- | Game Status
data GameStat = Lobby | Running | Aborted | Finished
  deriving (Show, Read, Eq)

newGame :: Text -> Text -> Int -> STM Game
newGame gameName gameMap numPlayers = return Game {gameState=Lobby,
                                                   gamePlayers=[],
                                                   ..}

-- | Messages sent by Client
data ClientMessage =
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

Prelude.concat <$> mapM (deriveJSON defaultOptions) [''ClientMessage,
                                             ''Game,
                                             ''GameStat,
                                             ''ServerMessage,
                                             ''Version]
