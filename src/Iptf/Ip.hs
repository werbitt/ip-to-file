{-# LANGUAGE OverloadedStrings #-}

module Iptf.Ip where

import qualified Data.Text as T
import           Data.Attoparsec.Text
import qualified Data.ByteString.Char8 as B
import           Data.List (intersperse)
import           Data.Word

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

showIP :: IP -> String
showIP (IP d1 d2 d3 d4) =
  let digits = map show [d1, d2, d3, d4]
  in concat $ intersperse "."  digits
