-- Copyright 2015-2015 the openage authors. See copying.md for legal info.

{-| Version information for this software.
 - mainly used for compatibility checking.
 -}
module Version where

import qualified Data.Aeson as JSON

import qualified Data.Version as Ver
import qualified Paths_openage_masterserver as Prg

newtype Version = Version { getVersion :: Ver.Version }
                deriving (Eq, Ord, Show, Read)

instance JSON.FromJSON Version where
  parseJSON jsn = do
    ver <- JSON.parseJSON jsn
    return (Version (Ver.makeVersion ver))

instance JSON.ToJSON Version where
  toJSON (Version (Ver.Version v _)) = JSON.toJSON v


-- | Software version, fetched from cabal.
version :: Version
version = Version Prg.version

-- | Create a version from a list of ints.
-- [0, 1, 0] will become v0.1.0
makeVersion :: [Int] -> Version
makeVersion vx = Version (Ver.makeVersion vx)

showVersion :: Version -> String
showVersion v = Ver.showVersion (getVersion v)

-- | Software identification string
identifier :: String
identifier = "openage-masterserver"

-- | Combined version representation
programVersion :: String
programVersion = identifier ++ " " ++ (showVersion version)
