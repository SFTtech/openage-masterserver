{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
 -Copyright 2016-2016 the openage authors.
 -See copying.md for legal info.
 -}
module Protocol where

import Data.Aeson.TH
import Data.Version
import Data.Text(Text)
import Database.Persist.TH

-- | Game Status
data GameStat = Lobby | Running | Aborted | Finished
  deriving (Show, Read, Eq)
derivePersistField "GameStat"

-- | Peer Operation Type
data PeerType = Server | Client | Masterserver
  deriving (Show, Read, Eq)

-- | Messages sent by Client
data ClientMessage =
  GameInit {
  name :: Text,
  maxPlayers :: Int,
  state :: GameStat
  } |
  VersionMessage {
  peerSoftware :: Text,
  peerType :: PeerType,
  peerProtocolVersion :: Version
  } |
  LoginMessage {
  name :: Text,
  password :: Text
  } deriving (Show, Read, Eq)

concat <$> mapM (deriveJSON defaultOptions) [''ClientMessage, ''GameStat, ''PeerType, ''Version]
