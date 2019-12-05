module LoadBalance.Strategies where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import qualified Network.Socket         as NS

data Strategies
  = ROUND_ROBIN
  | LEAST_CONN

getBalancer :: Strategies -> IO ([NS.Socket] -> IO NS.Socket)
getBalancer balancingStrategy = do
  case balancingStrategy of
    ROUND_ROBIN -> do
      counter <- newTVarIO 0
      return (roundRobinBalancer counter)
    _ -> return (\sockets -> return $ head sockets)

roundRobinBalancer :: TVar Int -> [NS.Socket] -> IO NS.Socket
roundRobinBalancer counter sockets = do
  idx <- readTVarIO counter
  atomically $ writeTVar counter (getIdx (idx + 1) (length sockets))
  return $ sockets !! idx
  where
    getIdx value max = value `mod` max
