{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
 -Copyright 2016-2016 the openage authors.
 -See copying.md for legal info.
 -}
module DBSchema where

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Data.Text
import Control.Monad.Logger
import Control.Monad.IO.Class
import Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Player
    username Text
    UniqueUsername username
    password Text
    deriving Show
|]

addPlayer :: Player -> Bool
addPlayer = undefined

getPlayer :: Text -> IO(Maybe (Entity Player))
getPlayer pName = do
  (con, _)<- loadConf
  conStr <- loadCfgStr con
  runPost conStr $ getBy $ UniqueUsername pName

runPost conStr action = runStderrLoggingT $ withPostgresqlPool conStr 10 $ \pool ->
  liftIO $ flip runSqlPersistMPool pool $ do
    runMigration migrateAll
    action
