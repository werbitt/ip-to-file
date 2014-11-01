{-# Language OverloadedStrings #-}

module Iptf.Ip where

import           Control.Concurrent.Async
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as L
import           GHC.Exception
import           Network.HTTP.Conduit

testUrls :: [String]
testUrls = [ "http://ipecho.net/plain"
       , "http://icanhazip.com"
       , "fuzz"
       ]

ip :: IO L.ByteString
ip = simpleHttp "http://ipecho.net/plain"

mkRequests :: [String] -> [Either SomeException Request]
mkRequests urls = map parseUrl urls

asyncIp :: [Request] -> IO L.ByteString
asyncIp rs  = asyncRequests rs >>= waitAnyCancel >>= return . snd

asyncRequests :: [Request] -> IO [Async L.ByteString]
asyncRequests rs = sequence $ map (async . get) rs
  where
    get req =  liftIO $ fmap responseBody $ withManager (httpLbs req)

goodRequests :: [Either SomeException Request] -> [Request]
goodRequests = foldr go []
  where go (Left _) xs = xs
        go (Right x) xs = x:xs
