module Main where

import           Control.Applicative (pure)
import qualified Data.ByteString.Lazy.Char8 as L
import           Iptf.Hosts
import           Iptf.Ip
import           Iptf.Options
import           Network.HTTP.Conduit (simpleHttp)
import           System.IO
import qualified Data.Text as T


ipFromFile :: FilePath -> IO (Maybe IP)
ipFromFile f = handle (const $ return Nothing :: IOError -> IO (Maybe IP)) $
   B.readFile f  >>= \b -> return $ case getIP b of
                                     Left _  -> Nothing
                                     Right i -> Just i

ipFromWeb :: String -> IO (Either String IP)
ipFromWeb u = simpleHttp u >>= return . getIP . L.toStrict

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
