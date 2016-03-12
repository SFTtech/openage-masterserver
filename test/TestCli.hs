{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import Control.Concurrent.Async
import System.Environment
import Text.Printf
import Data.Version
import Data.Aeson
import Data.ByteString.Lazy as BL
import Data.ByteString.Char8 as B
import System.IO
import Data.Text as TE
import Data.Text.IO as T
import Network
import Protocol as P

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  case args of
    [host, port] -> do
      handle <- connectTo host $ PortNumber
        $ fromIntegral (read port :: Int)
      handleVersion handle
      getCredentials handle
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

getCredentials :: Handle -> IO ()
getCredentials handle = do
  printf "Enter Login Credentials:\n"
  printf "Name: \n"
  name <- T.getLine
  printf "Password: \n"
  pass <- T.getLine
  sendLogin name pass handle
  handleAnswer handle

handleAnswer :: Handle -> IO ()
handleAnswer handle = do
  logAns <- B.hGetLine handle
  case (decode . BL.fromStrict) logAns of
    Just P.Message{..} -> printf "Message: %s\n" messageString
    Just P.Error{..} -> printf "Error: %s\n" errorString
    Just GameInfoAnswer{..} -> printFormattedGame game
    Just GameQueryAnswer{..} -> printFormattedGames gameList
    _ -> printf "Error: Decoding error.\n"

mainLoop :: Handle -> IO ()
mainLoop handle = do
  _ <- race (forever $ handleAnswer handle) (handleLobbyInput handle)
  return ()

handleLobbyInput :: Handle -> IO ()
handleLobbyInput handle = do
  input <- T.getLine
  case TE.words input of
    ["playerConfig", civ, team, rdy] -> do
      sendEncoded handle (PlayerConfig civ ((read . TE.unpack) team)
                          ((read . TE.unpack) rdy))
      handleLobbyInput handle
    ["gameVictory"] -> do
      sendEncoded handle (GameResultMessage Victory)
      handleLobbyInput handle
    ["gameDefeat"] -> do
      sendEncoded handle (GameResultMessage Defeat)
      handleLobbyInput handle
    ["gameJoin", name] -> do
      sendEncoded handle $ GameJoin name
      handleLobbyInput handle
    ["gameInfo"] -> do
      sendEncoded handle GameInfo
      handleLobbyInput handle
    ["gameInit", name, gameMap, players] -> do
      sendGameInit handle name gameMap $ (read. TE.unpack) players
      handleLobbyInput handle
    ["gameQuery"] -> do
      sendGameQuery handle
      handleLobbyInput handle
    ["gameLeave"] -> do
      sendEncoded handle GameLeave
      handleLobbyInput handle
    ["gameStart"] -> do
      sendEncoded handle GameStart
      handleLobbyInput handle
    ["exit"] ->
      printf "exiting testclient...\n"
    ["help"] -> do
      printCommands
      handleLobbyInput handle
    com:_ -> do
      printf "%s: not found.\n" com
      handleLobbyInput handle
    _ ->
      handleLobbyInput handle

printCommands :: IO ()
printCommands = do
  printf "Available Commands: \n"
  printf "\tgameInit NAME MAP NUMBERPLAYERS - Add new Game \n"
  printf "\tgameQuery - Show existing games\n"
  printf "\tgameLeave - leave Game, close if Host \n"
  printf "\texit - Close testclient \n"

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

sendLogin :: AuthPlayerName -> Text -> Handle -> IO ()
sendLogin name pass handle =
  sendEncoded handle $ Login name pass
