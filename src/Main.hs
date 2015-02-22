-- Copyright 2015-2015 the openage authors. See copying.md for legal info.

import Control.Concurrent
import System.Exit
import System.IO

import qualified Data.Version as Ver
import qualified Network.Socket as Sock
import qualified Paths_openage_masterserver as Prg
import qualified System.Environment as Env

{-
 - main entry file for the openage masterserver
 -}



main :: IO ()
main = do
  args <- Env.getArgs
  if length args == 1 then
    do
      let mode = args !! 0
      case mode of
       "help" -> usage >> exit
       "run" -> listenSock
       _ -> usage >> die
  else
    usage >> die


usage   = putStrLn ("openage master server " ++ (Ver.showVersion Prg.version)
                    ++ "\n available options:\n" ++
                    "  help: show this help\n" ++
                    "  run: run the server")
exit    = exitWith (ExitSuccess)
die     = exitWith (ExitFailure 1)

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
