-- Copyright 2015-2015 the openage authors. See copying.md for legal info.

module Server where

import Control.Concurrent
import System.IO

import qualified Network.Socket as Sock

listenSock :: IO ()
listenSock = do
  let port = 4242
  putStrLn "openage matchmaking and lobby server"
  putStrLn ("listening on port " ++ show port)
  sock <- Sock.socket Sock.AF_INET6 Sock.Stream 0
  Sock.setSocketOption sock Sock.ReuseAddr 1
  Sock.bind sock (Sock.SockAddrInet6 port 0 Sock.iN6ADDR_ANY 0)
  Sock.listen sock 2
  acceptLoop sock

-- handle connections from the socket
acceptLoop :: Sock.Socket -> IO ()
acceptLoop sock = do
  conn <- Sock.accept sock
  _ <- forkIO (runConn conn)
  acceptLoop sock

-- per-connection action
runConn :: (Sock.Socket, Sock.SockAddr) -> IO ()
runConn (sock, addr) = do
  putStrLn ("connection: " ++ show addr)
  hdl <- Sock.socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  hPutStrLn hdl "ohai"
  hClose hdl
