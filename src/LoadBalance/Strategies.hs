module LoadBalance.Strategies where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import qualified Network.Socket         as NS

data Strategies
  = ROUND_ROBIN
  | LEAST_CONN

data RequestState =
  RequestState
    { clientHost     :: String
    , clientPort     :: String
    , backendSockets :: [NS.Socket]
    }

getBalancer :: Strategies -> IO (RequestState -> IO NS.Socket)
getBalancer balancingStrategy = do
  case balancingStrategy of
    ROUND_ROBIN -> do
      counter <- newTVarIO 0
      return (roundRobinBalancer counter)
    _ -> return (\reqState -> return $ head (backendSockets reqState))

roundRobinBalancer :: TVar Int -> RequestState -> IO NS.Socket
roundRobinBalancer counter reqState = do
  let sockets = backendSockets reqState
  idx <- readTVarIO counter
  atomically $ writeTVar counter (getIdx (idx + 1) (length sockets))
  return $ sockets !! idx
  where
    getIdx value max = value `mod` max
