-- Copyright 2015-2015 the openage authors. See copying.md for legal info.

module Database where

import Text.Printf

import qualified Database.HDBC as DB
import qualified Database.HDBC.PostgreSQL as PG

import qualified Config as Cfg

-- http://www.postgresql.org/docs/devel/static/libpq-connect.html#LIBPQ-CONNSTRING
connstring :: Cfg.Config -> String
connstring cfg = printf "host=%s dbname=%s user=%s password=%s" h db u p
  where
    h  = Cfg.dbHost cfg
    db = Cfg.dbName cfg
    u  = Cfg.dbUser cfg
    p  = Cfg.dbPassword cfg

connect :: Cfg.Config -> IO PG.Connection
connect cfg = do
  putStrLn "trying to connect to database..."
  con <- PG.connectPostgreSQL (connstring cfg)
  putStrLn "connected!"
  return con


initialize :: Cfg.Config -> IO ()
initialize cfg = do
  con <- connect cfg
  putStrLn "TODO: apply DB schema"
  DB.disconnect con
  putStrLn "disconnected!"
  return ()
