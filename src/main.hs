-- Copyright 2015-2015 the openage authors. See copying.md for legal info.

import Control.Concurrent
import Network.Socket
import System.IO

{-
sends a simple ohai to a tcp socket on port 4242
-}

main :: IO ()
main = do
  putStrLn "openage matchmaking and lobby server"
  putStrLn "listening on port 4242..."
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
  listen sock 2
  mainLoop sock

-- handle connections from the socket
mainLoop :: Socket -> IO ()
mainLoop sock = do
  conn <- accept sock
  forkIO (runConn conn)
  mainLoop sock

-- per-connection action
runConn (sock, _) = do
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  hPutStrLn hdl "ohai"
  hClose hdl
