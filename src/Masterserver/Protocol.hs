{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

------------------------------------------------------------------------------
-- |
-- Copyright 2016-2016 the openage authors. See copying.md for legal info.
-- Module: Masterserver.Protocol
--
-- This Module defines the JSON protocol used in client-server
-- communicaton.
--
-- Messages sent to serverthreads are of type InMessage, messages sent
-- to the client are of type OutMessage.

------------------------------------------------------------------------------
module Masterserver.Protocol (

  -- Messages sent and received by server
  InMessage(..), OutMessage(..),

  -- * Game datatype
  Game(..), newGame,
  -- ** Helper types
  AuthPlayerName, GameName, GameStat, Participant(..), newParticipant

  ) where

import Control.Concurrent.STM
import Data.Aeson.TH
import Data.Map.Strict as Map
import Data.Text(Text)
import Data.Version
import Network

-- |Messages sent by Client
data InMessage =
  AddPlayer{
    name :: Text,
    pw :: Text
  } |
  Broadcast {content :: Text} |
  ChatIn {content :: Text} |
  ChatOut {
    origin :: AuthPlayerName,
    content :: Text
  } |
  Login {
    loginName :: AuthPlayerName,
    loginPassword :: Text
  } |
  Logout |
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
  GameOver  |
  GameStart |
  GameStartedByHost |
  PlayerConfig {
    playerCiv :: Text,
    playerTeam :: Int,
    playerReady :: Bool
  } |
  VersionMessage {
    peerProtocolVersion :: Version
  } deriving (Show, Read, Eq)

-- |Messages sent by Server
data OutMessage =
  GameStartAnswer {playerMap :: Map AuthPlayerName HostName} |
  GameQueryAnswer {gameList :: [Game]} |
  GameInfoAnswer {game :: Game} |
  Error {errorString :: Text} |
  Message {messageString :: Text}
  deriving Show

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

-- |Unique player account name
type AuthPlayerName = Text

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

Prelude.concat <$> mapM (deriveJSON defaultOptions) [''InMessage,
                                                     ''Participant,
                                                     ''Game,
                                                     ''GameStat,
                                                     ''OutMessage,
                                                     ''Version]
