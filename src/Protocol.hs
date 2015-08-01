-- Copyright 2015-2015 the openage authors. See copying.md for legal info.

module Protocol where

import Control.Monad

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as TXT

import qualified BiMap
import qualified Version as Ver


class NetMsg a where
  toByteString :: a -> BS.ByteString
  fromByteString :: BS.ByteString -> Maybe a

-- | Protocol negotiation message
data VersionMessage = VersionMessage {
  peerSoftware :: String,
  peerType :: PeerType,
  peerProtocolVersion :: Ver.Version
  } deriving Show

instance NetMsg VersionMessage where
  toByteString m = JSON.encode m
  fromByteString m = JSON.decode m

-- | Peer operation type
data PeerType = Server | Client | Masterserver deriving (Show, Ord, Eq)

peertype :: BiMap.BiMap PeerType String
peertype = BiMap.bimap [(Server, "server"),
                        (Client, "client"),
                        (Masterserver, "masterserver")]

-- | converting version messages from json
instance JSON.FromJSON VersionMessage where
  parseJSON (JSON.Object o) = VersionMessage <$>
                              o JSON..: (TXT.pack "peersoftware") <*>
                              (pure (typelookup peertype) <*> (o JSON..: (TXT.pack "peertype"))) <*>
                              o JSON..: (TXT.pack "protocol")
    where
      typelookup m s = case BiMap.biLookupR s m of
        Just t -> t
        Nothing -> error (show s ++ " not a valid peer type")

  parseJSON _ = mzero

instance JSON.ToJSON VersionMessage where
  toJSON (VersionMessage sw pt pv) = JSON.object [
    (TXT.pack "peersoftware") JSON..= sw,
    (TXT.pack "peertype") JSON..= (BiMap.biLookupL pt peertype),
    (TXT.pack "protocol") JSON..= pv
    ]


data CompatibilityMessage = CompatibilityMessage {
  versionCompatible :: Bool
  } deriving Show

instance JSON.FromJSON CompatibilityMessage where
  parseJSON (JSON.Object o) = CompatibilityMessage <$>
                              o JSON..: (TXT.pack "compatible")
  parseJSON _ = mzero

instance JSON.ToJSON CompatibilityMessage where
  toJSON (CompatibilityMessage c) = JSON.object [
    (TXT.pack "compatible") JSON..= c
    ]

instance NetMsg CompatibilityMessage where
  toByteString m = JSON.encode m
  fromByteString m = JSON.decode m


-- | Communication protocol version.
version :: Ver.Version
version = Ver.makeVersion [0, 0]

serverVersion :: VersionMessage
serverVersion = VersionMessage{peerSoftware=(Ver.identifier), peerType=(Masterserver), peerProtocolVersion=(version)}

pack :: (NetMsg a) => a -> BS.ByteString
pack msg = toByteString msg

unpack :: (NetMsg a) => BS.ByteString -> Maybe a
unpack msg = fromByteString msg
