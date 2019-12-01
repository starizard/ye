module Config where

import           Options.Applicative

data Config =
  Config
    { listenPort  :: String
    , remoteHosts :: String
    , debug       :: Bool
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
  switch (long "debug" <> short 'd' <> help "Enable debug logging")

instance Show Config where
  show (Config l rh d) =
    "listenPort: " <> l <> ", remoteHosts: " <> rh <> ", debug: " <> show d
