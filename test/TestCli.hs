{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
------------------------------------------------------------------------------
-- |
-- Copyright 2016-2016 the openage authors. See copying.md for legal info.
--
-- Main entry file for a test-client
-- this client takes a host and a port as argument
-- and can send Messages defined in Protocol to the server

------------------------------------------------------------------------------
module Main where

import Data.Key
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
import Network.Socket

import Masterserver.Server
import Masterserver.Protocol as P

type Continue = Bool 

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  printInit
  case args of
    [host, port] -> do
      let hints = defaultHints { addrSocketType = Stream }
      addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock (addrAddress addr)
      printf "Connected\n"
      handle <- socketToHandle sock ReadWriteMode
      hSetBuffering handle NoBuffering
      handleVersion handle
      getSendCredentials handle
      mainLoop handle
    _ -> do
      printf "Please provide host and port"
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
    Just ChatOut{..} ->
      printf "%s: %s\n" chatOutOrigin chatOutContent
    _ -> printf "Error: Decoding error."

mainLoop :: Handle -> IO ()
mainLoop handle = do
  ans <- async $ handleAnswer handle
  inp <- async $ handleLobbyInput handle
  -- Weird behavior, seems like it stuck (deadlocking?) when using
  -- waitEitherCancel or the equivalent 'race' function.
  result <- waitEither ans inp
  case result of
    Right True -> 
      printf "Exit requested, proceeding...\n"
    _ -> 
      mainLoop handle

handleLobbyInput :: Handle -> IO Continue
handleLobbyInput handle = go =<< TE.words <$> T.getLine
  where
    go ["exit"] =
      pure True
    go command = do
      handleGeneric command
      pure False

    handleGeneric ("chat":content) =
      sendEncoded handle $ ChatFromClient $ TE.unwords content
    handleGeneric ["playerconfig", civ, team, rdy] =
      sendEncoded handle (PlayerConfig civ ((read . TE.unpack) team)
                                           ((read . TE.unpack) rdy))
    handleGeneric ["gameconfig", gMap, mode, num] = 
      sendEncoded handle (GameConfig gMap mode ((read . TE.unpack) num))
    handleGeneric ["gameover"] = sendEncoded handle GameOver
    handleGeneric ["join", name] = sendEncoded handle $ GameJoin name
    handleGeneric ["info"] = sendEncoded handle GameInfo
    handleGeneric ["init", name, gameMap, players] =
      sendGameInit handle name gameMap $ (read. TE.unpack) players
    handleGeneric ["query"] = sendGameQuery handle
    handleGeneric ["leave"] = sendEncoded handle GameLeave
    handleGeneric ["start"] = sendEncoded handle GameStart
    handleGeneric ["help"] = printCommands
    handleGeneric _ = printf "Command not found.\n"

printInit :: IO ()
printInit = do
  printf "---------------------------------------------------------\n"
  printf "- openage masterserver Testclient\n"
  printf "- Type \"help\" for more information and \"exit\" to exit the client\n"
  printf "---------------------------------------------------------\n"

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
  printf "Title\tMap\tPlayers\tHost\n"
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
