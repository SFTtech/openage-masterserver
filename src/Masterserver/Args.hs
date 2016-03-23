{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Copyright 2016-2016 the openage authors. See copying.md for legal info.
-- Module: Masterserver.Config
--
-- This Module defines the masterservers commandline argument parser.

-----------------------------------------------------------------------------
module Masterserver.Args where

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO as S

import Masterserver.Config

data Options = Options
  { optConfPath :: FilePath
  , optPort :: Maybe Int
  }

defaultOptions :: Options
defaultOptions = Options
  { optConfPath = "etc/openage/masterserver.yaml"
  , optPort = Nothing
  }

header :: String
header = "Usage: openage-masterserver [options]...\nOptions:\n"

options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "h" ["help"]   (NoArg printHelp)
    "Print help"
  , Option "c" ["config"] (ReqArg readConfPath "FILE")
    "config file to use"
  , Option "p" ["port"]   (ReqArg readPort "NUM")
    "port to use"
  ]

printHelp :: Options -> IO Options
printHelp _ = do
  S.hPutStrLn stderr (usageInfo header options)
  exitSuccess

readPort :: String -> Options -> IO Options
readPort arg opt = return opt { optPort = Just (read arg) }

readConfPath :: String -> Options -> IO Options
readConfPath arg opt = return opt { optConfPath = arg }

parseOpts :: IO Config
parseOpts = do
  args <- getArgs
  case getOpt RequireOrder options args of
    (actions, [], [])
      -> do
      opts <- foldl (>>=) (return defaultOptions) actions
      buildConfig opts
    (_, nonOpts, [])
      -> error $ "unrecognized arguments: " ++ unwords nonOpts
    (_, _, msgs)
      -> error $ concat msgs ++ usageInfo header options

-- | Reads config file from config path specified in args and returns a config
-- with specified options or, if not given, options read from file
buildConfig :: Options -> IO Config
buildConfig Options{..} = do
  conf <- loadConf optConfPath
  maybe (return conf) (\p -> return (conf {serverPort=p})) optPort
