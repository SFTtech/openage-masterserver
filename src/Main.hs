-- Copyright 2015-2015 the openage authors. See copying.md for legal info.


{-| main entry file for the openage masterserver
 - this server will listen on a tcp socket
 - and provide a funny API for gameservers and clients
 - to start communicating with each other.
 -}
module Main where

import qualified System.Environment as Env

import qualified Args
import qualified Config as Cfg
import qualified Database as DB
import qualified Server as Srv
import qualified Version as Ver

-- | available program run modes.
data Action = Serve | InitDB

-- | available command line options for running modes.
opts :: [Args.OptEntry]
opts = [("help",   ("show things that may help you", Args.NoOpt (putStrLn help >> Args.exit))),
        ("run",    ("run the server", Args.CmdOption "configfile" (run Serve))),
        ("initdb", ("initialize the database", Args.CmdOption "configfile" (run InitDB)))]

-- | generate a string that displays the command line usage of this server.
usage :: String
usage = "== " ++ Ver.programVersion
        ++ " ==\n\n"
        ++ "usage: binary <mode> <mode options>\n\n"
        ++ Args.modeString opts "available modes:\n"

-- | help about this program.
help :: String
help = "-=[ openage master server ]=-\n"
       ++ "  `<http://openage.sft.mx>´\n\n"
       ++ "Copyright (C) 2015 the openage authors, GNU AGPLv3 or later.\n\n"
       ++ "What does this program do?\n"
       ++ " * registry for game lobbies\n"
       ++ " * mediating between dedicated servers and game clients\n"
       ++ "\n"
       ++ "For more information and help,\n"
       ++ "visit our IRC channel #sfttech on freenode.net"

-- | entry point for this program.
main :: IO ()
main = do
  args <- Env.getArgs
  Args.parseArgs args opts usage

-- | entry point of all actions the server can perform.
run :: Action -> String -> IO ()
run mode cfgfile = do
  cfg <- Cfg.readConfig cfgfile
  case mode of
    Serve -> Srv.listenSock cfg
    InitDB -> DB.initialize cfg
