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

addPlayer :: Text -> BC.ByteString -> IO (Maybe (PO.Key Player))
addPlayer name pw =
  runPost $ PO.insertUnique $ Player name pw

getPlayer :: Text -> IO(Maybe (PO.Entity Player))
getPlayer pName =
  runPost $ PO.getBy $ UniqueUsername pName

runPost :: SqlPersistT IO a -> IO a
runPost action = do
  conf <- getPostgresConf
  pool <- createPoolConfig conf
  PO.runPool conf action pool
