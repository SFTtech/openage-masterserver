-- Copyright 2015-2015 the openage authors. See copying.md for legal info.


-- | database schema definition
module DBSchema where

-- | postgresql schema init for the openage master server database
schema = unlines [
  "SET client_encoding = 'UTF8';",

  -- table for currently registered gameservers
  "CREATE TABLE servers (",
  "  ip inet NOT NULL,",
  "  port inet NOT NULL,",
  "  name text NOT NULL,",
  "  last_alive timestamp NOT NULL,",
  "  PRIMARY KEY (ip, port)",
  ");"
  ]
