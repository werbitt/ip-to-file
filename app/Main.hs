module Main where

import           Iptf         (ipFromWeb, updateHostsFile)
import           Iptf.Options (Options (..), getOptions)

main :: IO ()
main = do
  opts <- getOptions
  ip <- ipFromWeb $ url opts
  case ip of
    Left e -> putStrLn e
    Right ip' -> updateHostsFile ip' (hostname opts) (path opts)
