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
       ( getPostgresConf
       , getPort
       , getVersion
       ) where

import Data.Aeson.TH
import Data.Yaml
import Database.Persist.Postgresql

-- | Config datatype used for parsing.
data Config = Config
  { acceptedVersion :: ![Int] -- ^ Client version accepted by server
  , port :: !Int              -- ^ Port to run server on
  , database :: !Value        -- ^ Database connection info
  } deriving Show

-- | Derives aeson toJSON and fromJSON instances for Config.
$(deriveJSON defaultOptions ''Config)

-- | Get PostgresConf from yaml file.
getPostgresConf :: IO PostgresConf
getPostgresConf = do
  Just yaml <- decodeFile "etc/openage/masterserver.yaml"
  conf <- parseMonad loadConfig $ database yaml
  applyEnv (conf :: PostgresConf)

-- | Get port from yaml file.
getPort :: IO Int
getPort = do
  Just yaml <- decodeFile "etc/openage/masterserver.yaml"
  return $ port yaml

-- | Get accepted version from yaml file.
getVersion :: IO [Int]
getVersion = do
  Just yaml <- decodeFile "etc/openage/masterserver.yaml"
  return $ acceptedVersion yaml

