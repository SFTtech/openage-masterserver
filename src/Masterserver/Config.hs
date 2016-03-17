{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
------------------------------------------------------------------------------
-- |
-- Copyright 2016-2016 the openage authors. See copying.md for legal info.
-- Module: Masterserver.Config
--
-- This Module defines the masterservers config parser.

------------------------------------------------------------------------------
module Masterserver.Config (
  getPostgresConf,
  getPort,
  getVersion
  )where

import Prelude hiding (concat, lookup, putStrLn)
import Data.Aeson.TH
import Data.Yaml
import Database.Persist.Postgresql

data Config = Config {
  acceptedVersion :: [Int],
  port :: Int,
  database :: Value
  } deriving Show

$(deriveJSON defaultOptions ''Config)

getPostgresConf :: IO PostgresConf
getPostgresConf = do
  Just yaml <- decodeFile "etc/openage/masterserver.yaml"
  conf <- parseMonad loadConfig $ database yaml
  applyEnv (conf :: PostgresConf)

getPort :: IO Int
getPort = do
  Just yaml <- decodeFile "etc/openage/masterserver.yaml"
  return $ port yaml

getVersion :: IO [Int]
getVersion = do
  Just yaml <- decodeFile "etc/openage/masterserver.yaml"
  return $ acceptedVersion yaml

