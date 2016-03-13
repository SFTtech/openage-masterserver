{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |Copyright 2016-2016 the openage authors.
-- See copying.md for legal info.
--
-- Main entry file for a test-client
-- this client takes a host and a port as argument
-- and can send Messages defined in Protocol to the server
module Main where

import Data.Key
import Control.Monad
import Control.Concurrent.Async
import System.Environment
import Text.Printf
import Data.Version
import Data.Aeson
import Data.Map as Map
import Data.ByteString.Lazy as BL
import Data.ByteString.Char8 as B
import System.IO
import Data.Text as TE
import Data.Text.IO as T
import Network

import Server
import Protocol as P

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  printInit
  case args of
    [host, port] -> do
      handle <- connectTo host $ PortNumber
        $ fromIntegral (read port :: Int)
      hSetBuffering handle NoBuffering
      handleVersion handle
      getSendCredentials handle
      mainLoop handle
    _ -> do
      T.putStrLn "Please provide host and port"
      return ()

handleVersion :: Handle -> IO ()
handleVersion handle = do
  let myVer = makeVersion [1,0,0]
  printf "Sending version: %s\n" $ showVersion myVer
  sendEncoded handle $ VersionMessage myVer
  handleAnswer handle

getSendCredentials :: Handle -> IO ()
getSendCredentials handle = do
  input <- T.getLine
  case TE.words input of
    ["addplayer",name,pass] -> do
      sendEncoded handle $ AddPlayer name pass
      handleAnswer handle
      getSendCredentials handle
    ["login" ,name ,pass] -> do
      sendEncoded handle $ Login name pass
      handleAnswer handle
    ["help"] -> do
      printCommands
      getSendCredentials handle
    _ -> do
      printf "Command not found.\n"
      getSendCredentials handle

handleAnswer :: Handle -> IO ()
handleAnswer handle = do
  logAns <- B.hGetLine handle
  case (decode . BL.fromStrict) logAns of
    Just P.Message{..} -> printf "Message: %s\n" messageString
    Just P.Error{..} -> printf "Error: %s\n" errorString
    Just GameInfoAnswer{..} -> printFormattedGame game
    Just GameQueryAnswer{..} -> printFormattedGames gameList
    Just GameStartAnswer{..} -> printFormattedGameStart playerMap
    _ -> T.putStrLn "Error: Decoding error."

mainLoop :: Handle -> IO ()
mainLoop handle = do
  _ <- race (forever $ handleAnswer handle) (handleLobbyInput handle)
  return ()

handleLobbyInput :: Handle -> IO ()
handleLobbyInput handle = do
  input <- T.getLine
  case TE.words input of
    ["playerconfig", civ, team, rdy] -> do
      sendEncoded handle (PlayerConfig civ ((read . TE.unpack) team)
                          ((read . TE.unpack) rdy))
      handleLobbyInput handle
    ["gameconfig", gMap, mode, num] -> do
      sendEncoded handle (GameConfig gMap mode ((read . TE.unpack) num))
      handleLobbyInput handle
    ["gameover"] -> do
      sendEncoded handle GameOver
      handleLobbyInput handle
    ["join", name] -> do
      sendEncoded handle $ GameJoin name
      handleLobbyInput handle
    ["info"] -> do
      sendEncoded handle GameInfo
      handleLobbyInput handle
    ["init", name, gameMap, players] -> do
      sendGameInit handle name gameMap $ (read. TE.unpack) players
      handleLobbyInput handle
    ["query"] -> do
      sendGameQuery handle
      handleLobbyInput handle
    ["leave"] -> do
      sendEncoded handle GameLeave
      handleLobbyInput handle
    ["start"] -> do
      sendEncoded handle GameStart
      handleLobbyInput handle
    ["exit"] ->
      printf "exiting testclient...\n"
    ["help"] -> do
      printCommands
      handleLobbyInput handle
    _ -> do
      printf "Command not found.\n"
      handleLobbyInput handle

printInit :: IO ()
printInit = do
  T.putStrLn "---------------------------------------------------------"
  T.putStrLn "- openage masterserver Testclient"
  T.putStrLn "- Type \"help\" for more information and \"exit\" to exit the client"
  T.putStrLn "---------------------------------------------------------"

printCommands :: IO ()
printCommands = do
  printf "Available Commands: \n"
  printf "Login:\n"
  printf "\taddplayer NAME PASS - add player to server\n"
  printf "\tlogin NAME PASS - Login to server\n"
  printf "Lobby:\n"
  printf "\tquery - Show existing games\n"
  printf "\tinit NAME MAP NUMBERPLAYERS - Add new Game \n"
  printf "\tjoin NAME - join Gamelobby \n"
  printf "Gamelobby:\n"
  printf "\tinfo - show info about current game \n"
  printf "\tleave - leave Game, close if Host \n"
  printf "\tstart - Only Host: start the game, all Players need to be ready.\n"
  printf "\tplayerConfig Text:CIV Int:TEAM Bool:READY-\
         \ change Players settings.\n"
  printf "Ingame:\n"
  printf "\tleave - leave Game, close if Host \n"
  printf "\tgameover - Only Host: send gameover, terminates game.\n"
  printf "General:\n"
  printf "\texit - Close testclient \n"

printFormattedGameStart :: Map.Map AuthPlayerName HostName -> IO ()
printFormattedGameStart pMap = do
  printf "Gameinfo to start p2p:\n"
  mapWithKeyM_ (printf "\t%s: %s\n") pMap

printFormattedGame :: Game -> IO ()
printFormattedGame Game{..} = do
  printf "Name: %s\n" gameName
  printf "Map: %s\n" gameMap
  printf "MaxPlayers: %d\n" numPlayers
  printf "Gamehost: %s\n" gameHost
  printf "Team:\tName:\tCivilization:\tReady:\n"
  mapM_ printFormattedPart gamePlayers

printFormattedPart :: Participant -> IO ()
printFormattedPart Participant{..} =
  printf "%d\t%s\t%s\t\t%s\n" parTeam parName parCiv $ show parReady

printFormattedGames :: [Game] -> IO ()
printFormattedGames games = do
  T.putStr "Title\tMap\tPlayers\tHost\n"
  mapM_ printGame games
    where
      printGame Game{..} =
        printf "%s\t%s\t%d\t%s\n" gameName gameMap numPlayers gameHost

sendGameQuery :: Handle -> IO ()
sendGameQuery handle =
  sendEncoded handle GameQuery

sendGameInit :: Handle -> GameName -> Text -> Int -> IO ()
sendGameInit handle name gameMap players =
  sendEncoded handle $ GameInit name gameMap players
