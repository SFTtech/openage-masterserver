{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}

------------------------------------------------------------------------------
-- |
-- Copyright 2016-2016 the openage authors. See copying.md for legal info.
-- Module: Masterserver: Masterserver.Database
--
-- This Module defines the database scheme used by the masterserver and
-- provides database access functions

------------------------------------------------------------------------------
module Masterserver.Database where

import Data.ByteString.Char8 as BC
import Database.Persist
import Database.Persist.Postgresql as PO
import Database.Persist.TH
import Data.Text

import Masterserver.Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Player
    username Text
    UniqueUsername username
    password ByteString
    deriving Show
|]

-- | Add Player to table player
addPlayer :: Text                       -- ^ Players unique account-name
          -> BC.ByteString              -- ^ Players salted password hash
          -> IO (Maybe (PO.Key Player)) -- ^ Resulting Player
addPlayer name pw =
  runPost $ PO.insertUnique $ Player name pw

-- | Get Player by name
getPlayer :: Text                         -- ^ Players unique name
          -> IO(Maybe (PO.Entity Player)) -- ^ Persist Entity for Player
getPlayer pName =
  runPost $ PO.getBy $ UniqueUsername pName

-- | Run a persist transaction with config credentials
runPost :: SqlPersistT IO a -- ^ Database access action
        -> IO a             -- ^ actions result
runPost action = do
  conf <- getPostgresConf
  pool <- createPoolConfig conf
  PO.runPool conf action pool
