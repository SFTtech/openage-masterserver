{-# LANGUAGE OverloadedStrings #-}

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

data Options = Options  { optConfPath :: FilePath}

startOptions :: Options
startOptions = Options  { optConfPath = "etc/openage/masterserver.yaml"
                        }

options :: [ OptDescr (Options -> IO Options) ]
options =
     [ Option "c" ["config"]
        (ReqArg
            (\arg opt -> return opt { optConfPath = arg })
            "FILE")
        "Config path"
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                S.hPutStrLn stderr (usageInfo prg options)
                exitSuccess))
        "Show help"
    ]

