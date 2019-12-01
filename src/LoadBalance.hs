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
import           Data.List.Split


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
mainLoop config sock = do
  let backends =  (splitOn ":") <$> (splitOn "," (getHost config))
  forever $ do
    (conn, peer) <- accept sock
    putStrLn $ "Connection from peer: " <> show peer
    downstreamChannel <- createChannel
    upstreamChannel <- createChannel

    backendSockets <- traverse (connectBackend config) backends
    mapM_ (\backendSock-> do
              -- Send message from backend to channel
              forkIO $ messageReader backendSock upstreamChannel
              -- Send message from channel to backend
              forkIO $ drainChannelToSocket downstreamChannel backendSock
          ) backendSockets

    forkIO $ messageReader conn downstreamChannel
    -- Draining channels from upstream - downstream & vice versa
    forkIO $ drainChannelToSocket upstreamChannel conn

  where connectBackend config [host, port] = connectToServer config host port
        connectBackend config [host] = connectToServer config host "80"
        connectBackend config _ = error "Host not supplied"


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
