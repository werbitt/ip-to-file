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
