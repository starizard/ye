module LoadBalance.Strategies where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Crypto.Hash
import qualified Data.ByteString.Internal as B

-- import qualified Data.ByteString.Lazy.Internal as BL
import qualified Network.Socket           as NS

data Strategies
  = ROUND_ROBIN
  | LEAST_CONN
  | SOURCE_IP_HASH

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
    SOURCE_IP_HASH -> return hashIPBalancer
    _ -> return (\reqState -> return $ head (backendSockets reqState))

hashIPBalancer :: RequestState -> IO NS.Socket
hashIPBalancer reqState = do
  let sockets = backendSockets reqState
      hashedClientIP = hashIP $ (clientHost reqState) ++ (clientPort reqState)
      hashValue = foldl (+) 1 (fromEnum <$> hashedClientIP)
      targetServerIdx = hashValue `mod` (length sockets)
  return $ sockets !! targetServerIdx

roundRobinBalancer :: TVar Int -> RequestState -> IO NS.Socket
roundRobinBalancer counter reqState = do
  let sockets = backendSockets reqState
  idx <- readTVarIO counter
  atomically $ writeTVar counter (getIdx (idx + 1) (length sockets))
  return $ sockets !! idx
  where
    getIdx value max = value `mod` max

hashIP :: String -> String
hashIP plaintext =
  B.unpackChars $
  digestToHexByteString ((hash (B.packChars plaintext)) :: Digest MD5)
