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
import Database.Persist as P
import Database.Persist.Postgresql as PO
import Database.Persist.TH
import Data.Text
import Control.Monad.Logger
import Control.Monad.IO.Class

import Masterserver.Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Player
    username Text
    UniqueUsername username
    password ByteString
    deriving Show
|]

addPlayer :: Text -> BC.ByteString -> IO (Maybe (Key Player))
addPlayer name pw = do
  (con, _)<- loadConf
  conStr <- loadCfgStr con
  runPost conStr $ insertUnique $ Player name pw

getPlayer :: Text -> IO(Maybe (Entity Player))
getPlayer pName = do
  (con, _)<- loadConf
  conStr <- loadCfgStr con
  runPost conStr $ getBy $ UniqueUsername pName

runPost conStr action = runNoLoggingT $ withPostgresqlPool conStr 10
  $ \pool -> liftIO $ flip runSqlPersistMPool pool $ do
    runMigration migrateAll
    action

runMig conStr = runNoLoggingT $ withPostgresqlPool conStr 10
  $ \pool -> liftIO $ flip runSqlPersistMPool pool $
    runMigration migrateAll
