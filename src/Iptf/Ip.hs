{-# LANGUAGE OverloadedStrings #-}

module Iptf.Ip where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Attoparsec.Text
import           Data.List (intersperse)
import           Data.Word
import           Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy.Char8 as L

data IP = IP Word8 Word8 Word8 Word8 deriving (Show, Eq, Ord)

parseIP :: Parser IP
parseIP = do
  skipSpace
  d1 <- decimal
  _ <- char '.'
  d2 <- decimal
  _ <- char '.'
  d3 <- decimal
  _ <- char '.'
  d4 <- decimal
  return $ IP d1 d2 d3 d4

getIP :: T.Text -> Either String IP
getIP = parseOnly parseIP

showIP :: IP -> Text
showIP (IP d1 d2 d3 d4) =
  let digits = map show [d1, d2, d3, d4]
  in T.pack . concat $ intersperse "."  digits

ipFromWeb :: String -> IO (Either String IP)
ipFromWeb u = simpleHttp u >>= return . getIP . T.pack . L.unpack
