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
import Data.Aeson.TH
import Data.Version
import Data.Text(Text)
import Control.Concurrent.STM

-- |Unique player account name
type AuthPlayerName = Text

-- |Game datatype
-- It stores information about an open game
data Game = Game {
  gameHost :: AuthPlayerName,
  gameName:: GameName,
  gameMap :: Text,
  gameMode :: Text,
  numPlayers :: Int,
  gamePlayers :: Map AuthPlayerName Participant,
  gameState :: GameStat
  } deriving Show

type GameName = Text

-- |Game Status
data GameStat = Lobby | Running | Aborted | Finished
  deriving (Show, Read, Eq)

newGame :: GameName -> AuthPlayerName -> Text -> Int -> STM Game
newGame gameName gameHost gameMap numPlayers =
  return Game {gameState=Lobby,
               gamePlayers=Map.empty,
               gameMode="Deathmatch", ..}

-- |Game participant, players ingame settings
data Participant = Participant {
  parName :: AuthPlayerName,
  parCiv :: Text,
  parTeam :: Int,
  parReady :: Bool
  } deriving Show

newParticipant :: AuthPlayerName -> Bool -> Participant
newParticipant parName parReady = Participant{parCiv="Britain",
                                              parTeam=0, ..}

-- |Messages sent by Client
data InMessage =
  AddPlayer{
    name :: Text,
    pw :: Text
  } |
  Broadcast {content :: Text} |
  Login {
    loginName :: AuthPlayerName,
    loginPassword :: Text
  } |
  GameClosedByHost |
  GameConfig {
    gameConfMap :: Text,
    gameConfMode :: Text,
    gameConfPlayerNum :: Int
  } |
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

-- |Messages sent by Server
data OutMessage =
  GameQueryAnswer {gameList :: [Game]} |
  GameInfoAnswer {game :: Game} |
  Error {errorString :: Text} |
  Message {messageString :: Text}
  deriving Show

Prelude.concat <$> mapM (deriveJSON defaultOptions) [''InMessage,
                                                     ''Participant,
                                                     ''Game,
                                                     ''GameStat,
                                                     ''GameResult,
                                                     ''OutMessage,
                                                     ''Version]
