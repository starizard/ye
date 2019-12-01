module LoadBalance where

import           Config
import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Char8     as C
import qualified LoadBalance.Channel       as LBC
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import           Control.Exception
import           Data.List.Split


startLoadBalance :: Config -> IO ()
startLoadBalance config = do
  let hints = NS.defaultHints {NS.addrFlags = [NS.AI_PASSIVE], NS.addrSocketType = NS.Stream}
      backLog = 5
      port = listenPort config
  addr:_ <- NS.getAddrInfo (Just hints) Nothing $ Just port
  sock <- NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr)
  NS.setSocketOption sock NS.ReuseAddr 1
  NS.bind sock (NS.addrAddress addr)
  NS.listen sock backLog
  print $ "Listening on port " <> port
  dispatcher config sock

dispatcher :: Config -> NS.Socket -> IO ()
dispatcher config sock = do
  let backends =  (splitOn ":") <$> (splitOn "," (remoteHosts config))
  forever $ do
    (conn, peer) <- NS.accept sock
    putStrLn $ "Connection from peer: " <> show peer
    downstreamChannel <- LBC.createChannel
    upstreamChannel <- LBC.createChannel

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


messageReader :: NS.Socket -> LBC.MessageChannel -> IO ()
messageReader sock readChannel =
  forever $ catch (do
    msgBytes <- NSB.recv sock 4096
    let msg = C.unpack msgBytes
    if msg == ""
      then NS.close sock
      else LBC.pushMsgToChannel msg readChannel
   )  (\(e :: IOError) -> return ())


connectToServer :: Config -> String -> String -> IO (NS.Socket)
connectToServer config host port =  catch (do
  let upstream = host <> ":" <> port
      hints = NS.defaultHints { NS.addrSocketType = NS.Stream }
  addrinfos <- NS.getAddrInfo (Just hints) (Just host) (Just port)
  let serveraddr = head addrinfos
  server <- NS.socket (NS.addrFamily serveraddr) NS.Stream NS.defaultProtocol
  NS.connect server (NS.addrAddress serveraddr)
  putStrLn $ "connected to " <> upstream
  return server)
  (\(e::IOError) -> do
    let upstream = host <> ":" <> port
    putStrLn $ "reconnecting to " <> upstream
    threadDelay 1000000
    connectToServer config host port
  )


drainChannelToSocket :: LBC.MessageChannel -> NS.Socket -> IO ()
drainChannelToSocket channel destinationSocket = do
  putStrLn $ "draining channel worker started"
  forever $ do
    msg <- LBC.getMsgFromChannel channel
    putStrLn $ "Got from channel " <> msg
    let msgBytes = C.pack msg
    catch (NSB.send destinationSocket msgBytes)
      (\(e :: IOError) -> do
        putStrLn $ "drain channel send failed " ++ show e
        return 0
      )
