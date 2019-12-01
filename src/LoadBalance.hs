module LoadBalance where

import           Config
import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Char8     as C
import           LoadBalance.Channel
import           Network.Socket
import           Network.Socket.ByteString
import           Control.Exception
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan


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
    downstreamChannel <- createChannel
    upstreamChannel <- createChannel
    backendSock <- connectToServer config (getHost config) (getPort config)

    forkIO $ messageReader conn downstreamChannel
    forkIO $ messageReader backendSock upstreamChannel
    -- Draining channels from upstream - downstream & vice versa
    forkIO $ drainChannelToSocket downstreamChannel backendSock
    forkIO $ drainChannelToSocket upstreamChannel conn

  where getHost config = remoteHost config
        getPort config = remotePort config



messageReader :: Socket -> MessageChannel -> IO ()
messageReader sock readChannel =
  forever $ do
    msgBytes <- recv sock 4096
    let msg = C.unpack msgBytes
    if msg == ""
      then close sock
      else pushMsgToChannel msg readChannel


connectToServer :: Config -> String -> String -> IO (Socket)
connectToServer config host port =  catch (do
  let upstream = host <> ":" <> port
  let hints = defaultHints { addrSocketType = Stream }
  addrinfos <- getAddrInfo (Just hints) (Just host) (Just port)
  let serveraddr = head addrinfos
  server <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect server (addrAddress serveraddr)
  putStrLn $ "connected to " <> upstream
  return server)
  (\(e::IOError) -> do
    let upstream = host <> ":" <> port
    putStrLn $ "reconnecting to " <> upstream
    threadDelay 1000000
    connectToServer config host port
  )


drainChannelToSocket :: MessageChannel -> Socket -> IO ()
drainChannelToSocket channel destinationSocket = do
  putStrLn $ "draining channel worker started"
  forever $ do
    msg <- atomically $ readTChan channel
    putStrLn $ "Got from channel " <> msg
    let msgBytes = C.pack msg
    catch (send destinationSocket msgBytes)
      (\(e :: IOError) -> do
        putStrLn $ "drain channel send failed " ++ show e
        return 0
      )
