module Main where

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
  let name = (hostname opts)
  tryWebIP <- ipFromWeb (url opts)
  case tryWebIP of
   Left e -> putStrLn e
   Right webIP -> do
     tryHosts <- readHosts (path opts)
     case tryHosts of
      Left e -> putStrLn e
      Right hosts -> do
        let tryFileIP = ipForHostname name hosts
        case tryFileIP of
         Nothing -> writeFile (path opts) $ toText (update hosts name webIP)
         Just fileIP  ->
           if webIP == fileIP
           then return ()
           else writeFile (path opts) $ toText (update hosts name webIP)

main :: IO ()
main = getOptions >>= run
