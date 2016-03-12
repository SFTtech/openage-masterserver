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
import Data.Text as Te
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
    Just GameQueryAnswer{..} -> printFormattedGames gameList
    _ -> printf "Error: Decoding error.\n"

mainLoop :: Handle -> IO ()
mainLoop handle = do
  _ <- race (forever $ handleAnswer handle) (handleLobbyInput handle)
  return ()

handleLobbyInput :: Handle -> IO ()
handleLobbyInput handle = do
  T.putStr "?> "
  hFlush stdout
  input <- T.getLine
  case Te.words input of
    ["gameInit", name, gameMap, players] -> do
      sendGameInit handle name gameMap $ read $ Te.unpack players
      handleLobbyInput handle
    ["gameQuery"] -> do
      sendGameQuery handle
      handleLobbyInput handle
    ["gameLeave"] -> do
      sendEncoded handle GameLeave
      handleLobbyInput handle
    ["exit"] ->
      printf "exiting testclient...\n"
    ["help"] -> do
      printCommands
      handleLobbyInput handle
    _ ->
      handleLobbyInput handle

printCommands :: IO ()
printCommands = do
  printf "Available Commands: \n"
  printf "\tgameInit \"name\" \"map\" \"numberPlayers\" - Add new Game \n"
  printf "\tgameQuery - Show existing games\n"
  printf "\tgameLeave - leave Game, close if Host \n"
  printf "\texit - Close testclient \n"

printFormattedGames :: [Game] -> IO ()
printFormattedGames games = do
  printf "Title\tMap\tPlayers\tHost\n"
  mapM_ printGame games
    where
      printGame game@Game{..} =
        printf "%s\t%s\t%d\t%s\n" gameName gameMap numPlayers gameHost

sendGameQuery :: Handle -> IO ()
sendGameQuery handle =
  sendEncoded handle GameQuery

sendGameInit :: Handle -> Text -> Text -> Int -> IO ()
sendGameInit handle name gameMap players =
  sendEncoded handle $ GameInit name gameMap players

sendLogin :: Text -> Text -> Handle -> IO ()
sendLogin name pass handle =
  sendEncoded handle $ Login name pass
