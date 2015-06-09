-- Copyright 2015-2015 the openage authors. See copying.md for legal info.

{-| Version information for this software.
 - mainly used for compatibility checking.
 -}
module Version where

import qualified Data.Aeson as JSON

import qualified Data.Version as Ver
import qualified Paths_openage_masterserver as Prg

instance JSON.FromJSON Ver.Version where
  parseJSON jsn = do
    ver <- JSON.parseJSON jsn
    return (Ver.makeVersion ver)

instance JSON.ToJSON Ver.Version where
  toJSON (Ver.Version v _) = JSON.toJSON v


-- | Software version, fetched from cabal.
version :: Ver.Version
version = Prg.version

-- | Software identification string
identifier :: String
identifier = "openage-masterserver"

-- | Combined version representation
programVersion :: String
programVersion = identifier ++ " " ++ (Ver.showVersion version)
