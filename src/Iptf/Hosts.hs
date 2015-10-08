module Iptf.Hosts
       (
         updateHosts
       , Hostname
       , hostname
       ) where

import           Iptf.Hosts.Internal (Hostname, hostname, updateHostsFile)
import           Iptf.Hosts.IO       (readHosts, writeHosts)
import           Iptf.Ip.Internal    (IP)

updateHosts :: IP -> Hostname -> FilePath -> IO ()
updateHosts ip name file = do
  hosts <- readHosts file
  case hosts of
    Right h -> case updateHostsFile h ip name of
      Changed h' ->  writeHosts file h'
      Same _ -> return ()
    Left  e -> error e
