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

-- | Game Server registration Message
data GameInit = GameInit {
  name :: Text,
  maxPlayers :: Int,
  state :: GameStat
  } deriving (Show, Eq)

-- | Game Status
data GameStat = Lobby | Running | Aborted | Finished
  deriving (Show, Read, Eq)
derivePersistField "GameStat"

-- | Communication Protocol Version Message
data VersionMessage = VersionMessage {
  peerSoftware :: Text,
  peerType :: PeerType,
  peerProtocolVersion :: Version
  } deriving (Show, Eq)

-- | Peer Operation Type
data PeerType = Server | Client | Masterserver
  deriving (Show, Eq)

concat <$> mapM (deriveJSON defaultOptions) [''GameInit, ''GameStat, ''PeerType, ''VersionMessage, ''Version]
