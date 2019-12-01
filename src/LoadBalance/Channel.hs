module LoadBalance.Channel where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan ()

type MessageChannel = TChan String

createChannel :: IO (MessageChannel)
createChannel = atomically newTChan

pushMsgToChannel :: String -> MessageChannel -> IO ()
pushMsgToChannel msg channel = atomically $ writeTChan channel msg
