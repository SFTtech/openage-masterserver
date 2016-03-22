{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
------------------------------------------------------------------------------
-- |
-- Copyright 2016-2016 the openage authors. See copying.md for legal info.
-- Module: Masterserver.Config
--
-- This Module defines the masterservers config parser.

------------------------------------------------------------------------------
module Masterserver.Config
       ( Config(..)
       , loadConf
       ) where

import Data.Aeson.TH
import Database.Persist.Postgresql
import Data.Yaml

-- | Config datatype used for parsing.
data Config = Config
  { acceptedVersion :: ![Int] -- ^ Client version accepted by server
  , serverPort :: !Int              -- ^ Port to run server on
  , postgresConf :: !PostgresConf        -- ^ Database connection info
  } deriving Show

-- | Derives aeson fromJSON instances for Config.
$(deriveFromJSON defaultOptions ''Config)

loadConf :: FilePath -> IO Config
loadConf path = do
  eitherConf <- decodeFileEither path
  case eitherConf of
    Right conf -> return conf
    Left _ -> error "Failed loading Config"
