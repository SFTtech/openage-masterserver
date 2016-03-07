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

-- | Game Status
data GameStat = Lobby | Running | Aborted | Finished
  deriving (Show, Read, Eq)
derivePersistField "GameStat"

type GameName = Text

data Game = Game {
  gameName:: Text,
  gameMap :: Text,
  numPlayers :: Int,
  gameState :: GameStat
  } deriving Show

newGame :: Text -> Text -> Int -> STM Game
newGame gameName gameMap numPlayers = return Game {gameState=Lobby, ..}

-- | Messages sent by Client
data ClientMessage =
  PlayerQuery |
  GameQuery |
  GameInit {
    gameInitName :: Text,
    gameInitMap :: Text,
    maxPlayers :: Int
  } |
  GameJoin {
    gameId :: Text
  }
  deriving (Show, Read, Eq)

data VersionMessage =
  VersionMessage {
    peerSoftware :: Text,
    peerProtocolVersion :: Version
  } deriving (Show, Read, Eq)

data LoginMessage =
  Login {
    loginName :: Text,
    loginPassword :: Text
  } deriving (Show, Read, Eq)

-- | Messages sent by Server
data ServerMessage =
  GameQueryAnswer {gameList :: [Game]} |
  Error {errorString :: Text} |
  Message {messageString :: Text}
  deriving Show

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


Prelude.concat <$> mapM (deriveJSON defaultOptions) [''ClientMessage,
                                             ''Game,
                                             ''LoginMessage,
                                             ''GameStat,
                                             ''ServerMessage,
                                             ''Version]
