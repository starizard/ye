module Main where

import           Config
import           LoadBalance
import           Options.Applicative

main :: IO ()
main = do
  cliConfig <- execParser (info initFlags (fullDesc))
  putStrLn $ "Starting with flags: " <> show cliConfig
  startLoadBalance cliConfig
