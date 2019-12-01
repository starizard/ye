module Config where

import Options.Applicative

data Config = Config
  { listenPort :: String
  , remoteHost :: String
  , remotePort :: String 
  , debug :: Bool }

initFlags :: Parser Config
initFlags = Config
        <$> strOption
            ( long "listenPort"
            <> short 'l'
            <> metavar "4242"
            <> help "local port to listen on" )
        <*>  strOption
              ( long "remoteHost"
              <> short 'r'
              <> metavar "localhost"
              <> help "Target host" )
        <*>  strOption
              ( long "remotePort"
              <> short 'p'
              <> metavar "9001"
              <> help "Target port" )
        <*> switch
              ( long "debug"
              <> short 'd'
              <> help "Enable debug logging" )