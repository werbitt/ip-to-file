module Iptf.Hosts
       (
         updateHostsFile
       , Hostname
       , hostname
       ) where

import           Iptf.Hosts.Internal (Hostname, Modifiable (..), hostname,
                                      updateHfc)
import           Iptf.Hosts.IO       (readHosts, writeHosts)
import           Iptf.Ip.Internal    (IP)

updateHostsFile :: IP -> Hostname -> FilePath -> IO ()
updateHostsFile ip name file = do
  hosts <- readHosts file
  case hosts of
    Right h -> case updateHfc h ip name of
      Changed h' ->  writeHosts file h'
      Same _ -> return ()
    Left  e -> error e
