module Iptf where

import           Control.Applicative
import           Data.Text.IO (writeFile)
import           Iptf.Hosts
import           Iptf.Ip
import           Prelude hiding (writeFile)


run :: IO ()
run = do
  opts <- getOptions
  tryWebIP <- ipFromWeb (url opts)
  case tryWebIP of
   Right webIP -> do
     hosts <- getHosts (path opts)
     if pure webIP == ipForHostname (hostname opts) hosts
     then return ()
     else writeFile (path opts) $ toText (update hosts (hostname opts) webIP)
   Left e -> putStrLn e
