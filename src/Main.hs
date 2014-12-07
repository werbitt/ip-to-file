module Main where

import           Control.Applicative (pure)
import qualified Data.ByteString.Lazy.Char8 as L
import           Iptf.Hosts
import           Iptf.Ip
import           Iptf.Options
import           Network.HTTP.Conduit (simpleHttp)
import           System.IO
import qualified Data.Text as T

ipFromFile :: FilePath -> Hostname -> IO (Maybe IP)
ipFromFile f n = readHosts f >>= \result ->
  case result of
  Left _ -> return Nothing
  Right hs -> return $ ipForHostname hs n

ipFromWeb :: String -> IO (Either String IP)
ipFromWeb u = simpleHttp u >>= return . getIP . T.pack . L.unpack

run :: Options -> IO ()
run opts = do
  saved <- ipFromFile (path opts) (hostname opts)
  new   <- ipFromWeb  $ url opts
  do case new of
      Left  e -> print e
      Right i -> if saved == pure i
                 then return ()
                 else do
                   withFile (path opts) WriteMode $ \h ->
                     hPutStrLn h $ showIP i

main :: IO ()
main = getOptions >>= run
