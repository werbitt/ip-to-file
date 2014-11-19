module Iptf.Hosts where

import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import qualified Data.Text as T
import           Iptf.Ip

type Hosts = Map.Map IP (S.Set Hostname)
newtype Hostname = Hostname T.Text
data HostRecord = Host IP [Hostname]

readHosts :: FilePath -> IO Hosts
readHosts = undefined
