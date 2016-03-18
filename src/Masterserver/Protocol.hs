{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
-- | Better performance for strict records
{-# OPTIONS_GHC -funbox-strict-fields #-}
-- | Ignore orphaned instances for Version aeson instances
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

module Masterserver.Protocol
       ( InMessage(..)
       , OutMessage(..)
       , Game(..)
       , newGame
       , AuthPlayerName
       , GameName
       , Participant(..)
       , newParticipant
       )
       where

import Control.Concurrent.STM
import Data.Aeson.TH
import Data.Map.Strict as Map
import Data.Text(Text)
import Data.Version
import Network

-- | Messages received by server
data InMessage
  = AddPlayer {name :: !Text, pw :: !Text}
  | Broadcast {content :: !Text}
  | ChatFromClient {chatFromCContent :: !Text}
  | ChatFromThread
    { chatFromTOrign :: !AuthPlayerName
    , chatFromTContent :: !Text
    }
  | Login
    { loginName :: !AuthPlayerName
    , loginPassword :: !Text
    }
  | Logout
  | GameClosedByHost
  | GameConfig
    { gameConfMap :: !Text
    , gameConfMode :: !Text
    , gameConfPlayerNum :: !Int
    }
  | GameInfo
  | GameInit
    { gameInitName :: !GameName
    , gameInitMap :: !Text
    , maxPlayers :: !Int
    }
  | GameJoin {gameId :: !GameName}
  | GameLeave
  | GameQuery
  | GameOver
  | GameStart
  | GameStartedByHost
  | PlayerConfig
    { playerCiv :: !Text
    , playerTeam :: !Int
    , playerReady :: !Bool
    }
  | VersionMessage {peerProtocolVersion :: !Version}
  deriving (Show, Read, Eq)

-- | Messages sent by Server
data OutMessage
  = ChatOut
    { chatOutOrigin :: !AuthPlayerName
    , chatOutContent :: !Text
    }
  | GameStartAnswer {playerMap :: Map AuthPlayerName HostName}
  | GameQueryAnswer {gameList :: ![Game]}
  | GameInfoAnswer {game :: !Game}
  | Error {errorString :: !Text}
  | Message {messageString :: !Text}
  deriving Show

-- | Game datatype
-- It stores information about an open game
data Game = Game
  { gameHost :: !AuthPlayerName
  , gameName:: !GameName
  , gameMap :: !Text
  , gameMode :: !Text
  , numPlayers :: !Int
  , gamePlayers :: Map AuthPlayerName Participant
  } deriving Show

-- | Unique player account name
type AuthPlayerName = Text

-- | Unique game name
type GameName = Text

newGame :: GameName -> AuthPlayerName -> Text -> Int -> STM Game
newGame gameName gameHost gameMap numPlayers =
  return Game { gamePlayers=Map.empty
              , gameMode="Deathmatch", ..}

-- | Game participant, players ingame settings
data Participant = Participant
  { parName :: !AuthPlayerName
  , parCiv :: !Text
  , parTeam :: !Int
  , parReady :: !Bool
  } deriving Show

newParticipant :: AuthPlayerName -> Bool -> Participant
newParticipant parName parReady = Participant{ parCiv="Britain"
                                             , parTeam=0, ..}

Prelude.concat <$> mapM (deriveJSON defaultOptions) [''InMessage,
                                                     ''Participant,
                                                     ''Game,
                                                     ''OutMessage,
                                                     ''Version]
