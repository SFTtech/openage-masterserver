-- Copyright 2015-2015 the openage authors. See copying.md for legal info.

module Args where

import Text.Printf
import qualified System.Exit as Exit
import qualified Data.Map.Strict as Map
import qualified Util

data CmdOption = NoOpt (IO ()) | CmdOption String (String -> IO ())
data OptMap = Map String (String, CmdOption)
type OptEntry = (String, (String, CmdOption))


modeString :: [OptEntry] -> String -> String
modeString [] txt = txt
modeString (x:xs) txt =
  modeString xs (txt ++ printf "  %s%s: %s\n" modename options modedesc)
  where
    modename = fst x
    modedesc = (fst . snd) x
    options = case (snd . snd) x of
      NoOpt _ -> ""
      CmdOption desc _ -> printf " <%s>" desc

runMode :: CmdOption -> [String] -> IO ()
runMode opt args =
  case opt of
   NoOpt f -> f
   CmdOption optname f ->
     case Util.maybeElem 1 args of
      Just name -> f name
      Nothing -> error ("missing argument: " ++ optname)

parseArgs :: [String] -> [OptEntry] -> String -> IO ()
parseArgs args opts usage =
  case (Util.maybeElem 0 args) of
   Just mode -> do
     case Map.lookup mode (Map.fromList opts) of
      Just (_, opt) -> runMode opt args
      Nothing       -> faild
   Nothing -> faild
   where
     faild = putStrLn usage >> die

exit :: IO ()
exit = Exit.exitWith (Exit.ExitSuccess)

die :: IO ()
die = Exit.exitWith (Exit.ExitFailure 1)
