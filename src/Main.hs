module Main where

import qualified Data.ByteString.Lazy.Char8 as L
import           Iptf.Options
import           Iptf.Parse
import           Network.HTTP.Conduit (simpleHttp)
import           System.IO

run :: Options -> IO ()
run opts = do
  out <- openFile (path opts) WriteMode
  response <- simpleHttp $ url opts
  do case getIP (L.toStrict response) of
      Left e -> print e
      Right i -> hPutStrLn out $ showIP i
  hClose out

main :: IO ()
main = getOptions >>= run
