-- Copyright 2015-2015 the openage authors. See copying.md for legal info.

module Main where

import System.Exit

import qualified Data.Version as Ver
import qualified Paths_openage_masterserver as Prg
import qualified System.Environment as Env

import qualified Config as Cfg
import qualified Database as DB
import qualified Server as Srv



{-
 - main entry file for the openage masterserver
 -}


main :: IO ()
main = do
  args <- Env.getArgs
  if length args == 2 then
    do
      let mode     = args !! 0
      let cfgfile  = args !! 1

      cfg <- Cfg.readConfig cfgfile

      case mode of
       "help"    -> usage >> exit
       "run"     -> Srv.listenSock
       "initdb"  -> DB.initialize cfg
       _         -> usage >> die
  else
    usage >> die


usage   = putStrLn ("openage master server " ++ (Ver.showVersion Prg.version)
                    ++ "\nusage: binary <mode> config.file"
                    ++ "\navailable modes:\n"
                    ++ "  help:    show this help\n"
                    ++ "  initdb:  initialize the database schema\n"
                    ++ "  run:     run the server")
exit    = exitWith (ExitSuccess)
die     = exitWith (ExitFailure 1)
