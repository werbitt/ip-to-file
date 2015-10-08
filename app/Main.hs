module Main where

import           Iptf         (ipFromWeb, updateHosts)
import           Iptf.Options (Options (..), getOptions)

main :: IO ()
main = do
  opts <- getOptions
  ip <- ipFromWeb $ url opts
  case ip of
    Left e -> putStrLn e
    Right ip' -> updateHosts ip' (name opts) (path opts)
