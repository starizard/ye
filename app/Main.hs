module Main where

import Options.Applicative
import Config
import LoadBalance

main :: IO ()
main = do
  cliConfig <- execParser (info initFlags (fullDesc))
  startLoadBalance
