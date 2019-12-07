module Config where

import qualified LoadBalance.Strategies as LBS
import           Options.Applicative

data Config =
  Config
    { listenPort        :: String
    , remoteHosts       :: String
    , balancingStrategy :: String
    , debug             :: Bool
    }

initFlags :: Parser Config
initFlags =
  Config <$>
  strOption
    (long "listenPort" <> short 'l' <> metavar "4242" <>
     help "local port to listen on") <*>
  strOption
    (long "remoteHosts" <> short 'r' <> metavar "127.0.0.1:80,10.10.10.11:80" <>
     help "Target host") <*>
  strOption
    (long "balancingStrategy" <> short 't' <>
     metavar "ROUND_ROBIN, SOURCE_IP_HASH" <>
     help "Balancing strategy ") <*>
  switch (long "debug" <> short 'd' <> help "Enable debug logging")

instance Show Config where
  show (Config l rh b d) =
    "listenPort: " <> l <> ", remoteHosts: " <> rh <> ", balancingStrategy " <>
    b <>
    ", debug: " <>
    show d

balancingStrategy' config =
  case (balancingStrategy config) of
    "SOURCE_IP_HASH" -> LBS.SOURCE_IP_HASH
    "ROUND_ROBIN"    -> LBS.ROUND_ROBIN
    _                -> LBS.ROUND_ROBIN
-- TODO: make this decode/read the type instead of case match
