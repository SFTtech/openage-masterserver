-- Copyright 2015-2015 the openage authors. See copying.md for legal info.

module Server where

import Control.Concurrent
import System.IO
import Control.Monad

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as B8
import qualified Network.Socket as Sock

import qualified Config as Cfg
import qualified Protocol as Proto

-- | tcp accept queue count.
qsize = 4

-- | open tcp listening socket.
listenSock :: Cfg.Config -> IO ()
listenSock cfg = do
  let port = fromIntegral (Cfg.netPort cfg)
  putStrLn "openage matchmaking and lobby server"
  putStrLn ("listening on port " ++ show port)
  sock <- Sock.socket Sock.AF_INET6 Sock.Stream 0
  Sock.setSocketOption sock Sock.ReuseAddr 1
  Sock.bind sock (Sock.SockAddrInet6 port 0 Sock.iN6ADDR_ANY 0)
  Sock.listen sock qsize
  acceptLoop sock

-- | accept connections from the socket.
acceptLoop :: Sock.Socket -> IO ()
acceptLoop sock = do
  conn <- Sock.accept sock
  _ <- forkIO (runConn conn)
  acceptLoop sock

-- | per-connection action.
runConn :: (Sock.Socket, Sock.SockAddr) -> IO ()
runConn (sock, addr) = do
  putStrLn ("connection: " ++ show addr)
  hdl <- Sock.socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  putStrLn "checking version..."
  -- start communicating!
  _ <- versionCheck hdl
  hClose hdl

-- | perform compatibility check
versionCheck :: Handle -> IO Bool
versionCheck hdl = do
  putStrLn "sending mine..."
  _ <- BSL.hPut hdl (Proto.pack Proto.serverVersion)
  putStrLn "receiving theirs..."
  peerversion <- liftM B8.fromString (hGetLine hdl) >>=
                  \ver -> case Proto.unpack ver of
                    Just a -> return a
                    Nothing -> error "no peer version received"

  -- versions must match.
  BSL.hPut hdl (Proto.pack (Proto.CompatibilityMessage (compat peerversion)))

  putStrLn ("ohai " ++ show peerversion)
  return True

  where
    compat v = Proto.peerProtocolVersion v == Proto.version
