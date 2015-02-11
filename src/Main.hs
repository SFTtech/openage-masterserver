-- Copyright 2015-2015 the openage authors. See copying.md for legal info.

import Control.Concurrent
import Network.Socket
import System.IO

{-
 - sends a simple ohai to a tcp socket on port 4242
 -}

main :: IO ()
main = do
  let port = 4242
  putStrLn "openage matchmaking and lobby server"
  putStrLn $ "listening on port " ++ show port
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet port iNADDR_ANY)
  listen sock 2
  mainLoop sock

-- handle connections from the socket
mainLoop :: Socket -> IO ()
mainLoop sock = do
  conn <- accept sock
  _ <- forkIO (runConn conn)
  mainLoop sock

-- per-connection action
runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, addr) = do
  putStrLn $ "connection: " ++ show addr
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  hPutStrLn hdl "ohai"
  hClose hdl
