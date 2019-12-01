module Config where

import           Options.Applicative

data Config =
  Config
    { listenPort :: String
    , remoteHost :: String
    , remotePort :: String
    , debug      :: Bool
    }

initFlags :: Parser Config
initFlags =
  Config <$>
  strOption
    (long "listenPort" <> short 'l' <> metavar "4242" <>
     help "local port to listen on") <*>
  strOption
    (long "remoteHost" <> short 'r' <> metavar "127.0.0.1" <> help "Target host") <*>
  strOption
    (long "remotePort" <> short 'p' <> metavar "9001" <> help "Target port") <*>
  switch (long "debug" <> short 'd' <> help "Enable debug logging")

instance Show Config where
  show (Config l rh rp d) =
    "listenPort: " <> l <> ", remoteHost: " <> rh <> ", remotePort: " <> rp <>
    ", debug: " <>
    show d
