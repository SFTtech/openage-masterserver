-- Copyright 2015-2015 the openage authors. See copying.md for legal info.


{-| main entry file for the openage masterserver
 - this server will listen on a tcp socket
 - and provide a funny API for gameservers and clients
 - to start communicating with each other.
 -}
module Main where

import qualified Data.Version as Ver
import qualified Paths_openage_masterserver as Prg
import qualified System.Environment as Env

import qualified Args
import qualified Config as Cfg
import qualified Database as DB
import qualified Server as Srv


opts :: [Args.OptEntry]
opts = [("help",   ("show usage info",         (Args.NoOpt (putStrLn usage >> Args.exit)))),
        ("run",    ("run the server",          (Args.CmdOption "configfile" run))),
        ("initdb", ("initialize the database", (Args.CmdOption "configfile" initdb)))]

usage :: String
usage = ("openage master server " ++ (Ver.showVersion Prg.version)
         ++ "\nusage: binary <mode> <mode options>\n"
         ++ (Args.modeString opts "available modes:\n"))

main :: IO ()
main = do
  args <- Env.getArgs
  Args.parseArgs args opts usage

run :: String -> IO ()
run cfgfile = do
  cfg <- Cfg.readConfig cfgfile
  Srv.listenSock cfg

initdb :: String -> IO ()
initdb cfgfile = do
  cfg <- Cfg.readConfig cfgfile
  DB.initialize cfg
