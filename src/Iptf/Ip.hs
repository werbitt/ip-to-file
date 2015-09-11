module Iptf.Ip ( ipFromWeb
               , IP
               , fromText
               , toText) where

import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Text                  (pack)
import           Iptf.Ip.Internal
import           Network.HTTP.Conduit       (simpleHttp)

-- | ipFromWeb takes a url that returns only a text IP address.
-- Such as http://ipecho.net/plain
ipFromWeb :: String -> IO (Either String IP)
ipFromWeb u = simpleHttp u >>= return . fromText . pack . L.unpack
