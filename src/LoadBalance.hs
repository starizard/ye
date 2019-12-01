module LoadBalance where

import           Config
import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Char8     as C
import           Network.Socket
import           Network.Socket.ByteString

startLoadBalance :: Config -> IO ()
startLoadBalance config = do
  let hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}
      backLog = 5
      port = listenPort config
      workerCount = 1
  addr:_ <- getAddrInfo (Just hints) Nothing $ Just port
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  listen sock backLog
  print $ "Listening on port " <> port
  mainLoop config sock

mainLoop :: Config -> Socket -> IO ()
mainLoop config sock =
  forever $ do
    (conn, peer) <- accept sock
    putStrLn $ "Connection from peer: " <> show peer
    forkIO $ messageReader conn

messageReader :: Socket -> IO ()
messageReader sock =
  forever $ do
    msgBytes <- recv sock 4096
    let msg = C.unpack msgBytes
    if msg == ""
      then close sock
      else putStrLn $ "Got msg: " <> msg
