module Main where

import           Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import           Data.Text.IO (writeFile)
import           Iptf.Hosts
import           Iptf.Ip
import           Iptf.Options
import           Network.HTTP.Conduit (simpleHttp)
import           Prelude hiding (writeFile)

ipFromWeb :: String -> IO (Either String IP)
ipFromWeb u = simpleHttp u >>= return . getIP . T.pack . L.unpack

run :: Options -> IO ()
run opts = do
  tryWebIP <- ipFromWeb (url opts)
  case tryWebIP of
   Right webIP -> do
     hosts <- getHosts (path opts)
     if pure webIP == ipForHostname (hostname opts) hosts
     then return ()
     else writeFile (path opts) $ toText (update hosts (hostname opts) webIP)
   Left e -> putStrLn e

main :: IO ()
main = getOptions >>= run
