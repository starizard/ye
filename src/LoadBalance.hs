module LoadBalance where

import           Network.Socket

import           Config

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
  putStrLn "started load balancer"
