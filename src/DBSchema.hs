{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
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

import Database.Persist.TH
import Data.Text
import Protocol

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Game
    name Text
    maxPlayers Int
    state GameStat
    deriving Show
Player
    username Text
    Username username
    password Text
    deriving Show
|]
