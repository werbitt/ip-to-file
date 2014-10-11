{-# Language OverloadedStrings #-}

module Iptf.Ip where

import qualified Data.ByteString.Lazy.Char8 as L
import           GHC.Exception
import           Network.HTTP.Conduit



ip :: IO L.ByteString
ip = simpleHttp "http://ipecho.net/plain"

noisyParseUrl :: String -> IO (Maybe Request)
noisyParseUrl = noisyUrl . parseUrl

noisyUrl :: Either SomeException Request -> IO (Maybe Request)
noisyUrl (Left e) = putStrLn (show e) >> return Nothing
noisyUrl (Right r) = return (Just r)
