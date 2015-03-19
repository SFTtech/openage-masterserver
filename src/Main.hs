-- Copyright 2015-2015 the openage authors. See copying.md for legal info.


{- | main entry file for the openage masterserver
 - this server will listen on a tcp socket
 - and provide a funny API for gameservers and clients
 - to start communicating with each other.
 -}
module Main where

import System.Exit
import Text.Printf

import qualified Data.Map.Strict as Map
import qualified Data.Version as Ver
import qualified Paths_openage_masterserver as Prg
import qualified System.Environment as Env

import qualified Config as Cfg
import qualified Database as DB
import qualified Server as Srv

data CmdOption = NoOpt (IO ()) | CmdOption String (String -> IO ())
data OptMap = Map String (String, CmdOption)

opts = [("help",   ("show usage info", (NoOpt (usage >> exit)))),
        ("run",    ("run the server",  (CmdOption "configfile" run))),
        ("initdb", ("initialize the database", (CmdOption "configfile" initdb)))]

getElem idx xs =
  if idx > (length xs) - 1 then
    Nothing
  else
    Just (xs !! idx)

main :: IO ()
main = do
  args <- Env.getArgs
  case (getElem 0 args) of
   Just mode -> do
     let o = Map.lookup mode (Map.fromList opts)
     case o of
      Just (_, opt) ->
        runMode opt args
      Nothing ->
        usage >> die
   Nothing -> usage >> die

runMode :: CmdOption -> [String] -> IO ()
runMode opt args =
  case opt of
   NoOpt f -> f
   CmdOption _ f ->
     case getElem 1 args of
      Just name -> f name
      Nothing -> error "missing argument"

run :: String -> IO ()
run _ = do
  Srv.listenSock

initdb :: String -> IO ()
initdb cfgfile = do
  cfg <- Cfg.readConfig cfgfile
  DB.initialize cfg

usage = putStrLn ("openage master server " ++ (Ver.showVersion Prg.version)
                  ++ "\nusage: binary <mode> <mode options>\n"
                  ++ "available modes:\n"
                  ++ (modeString opts ""))

modeString [] txt = txt
modeString (x:xs) txt =
  modeString xs (txt ++ printf "  %s%s: %s\n" (modename) (options) (modedesc))
  where
    modename = fst x
    modedesc = (fst . snd) x
    options = case (snd . snd) x of
      NoOpt _ -> ""
      CmdOption desc _ -> printf " <%s>" desc

exit    = exitWith (ExitSuccess)
die     = exitWith (ExitFailure 1)
