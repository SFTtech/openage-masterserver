-- Copyright 2015-2015 the openage authors. See copying.md for legal info.

module Database where

import Text.Printf

import qualified Database.HDBC as DB
import qualified Database.HDBC.PostgreSQL as PG

import qualified Config as Cfg
import qualified DBSchema as Schema

-- | Create a libpq connection string from a database config.
-- <http://www.postgresql.org/docs/devel/static/libpq-connect.html#LIBPQ-CONNSTRING>
connstring :: Cfg.Config -> String
connstring cfg = printf "host=%s dbname=%s user=%s password=%s" h db u p
  where
    h  = Cfg.dbHost cfg
    db = Cfg.dbName cfg
    u  = Cfg.dbUser cfg
    p  = Cfg.dbPassword cfg

-- | Connect to a postgresql database with a given config.
connect :: Cfg.Config -> IO PG.Connection
connect cfg = do
  putStrLn "connecting to database..."
  con <- PG.connectPostgreSQL (connstring cfg)
  putStrLn "connected!"
  return con

-- | Apply the schema to the database to initialize it
initialize :: Cfg.Config -> IO ()
initialize cfg = do
  con <- connect cfg
  putStrLn "applying DB schema..."
  DB.runRaw con Schema.schema
  putStrLn "DB schema created."
  DB.commit con
  DB.disconnect con
  putStrLn "disconnected from DB!"
  return ()
