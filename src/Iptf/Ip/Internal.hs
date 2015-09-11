{-# LANGUAGE OverloadedStrings #-}

module Iptf.Ip.Internal
       (
         IP(..)
       , fromText
       , parseIP
       , toText
       ) where

import           Data.Attoparsec.Text (Parser, char, decimal, parseOnly,
                                       skipSpace)
import           Data.List            (intersperse)
import           Data.String          (IsString (..))
import           Data.Text            (Text, pack)
import           Data.Word            (Word8)

-- | IP is a IP4 style IP address that is a group of 4 digits
data IP = IP Word8 Word8 Word8 Word8 deriving (Show, Eq, Ord)

instance IsString IP where
  -- fromString :: String -> IP
  fromString ip = case fromText $ pack ip of
    Right x -> x
    Left  e -> error ("Can't parse IP fromString: " ++ e)

-- | parseIP is an Attoparsec parser for IP4 style IP addresses.
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

-- | fromText takes an IP address in text form, such as
-- 123.123.43.4, and if it can be parsed it returns an IP,
-- otherwise it returns the parsing error.
fromText :: Text -> Either String IP
fromText = parseOnly parseIP

-- | toText takes an IP and converts it to text
toText :: IP -> Text
toText (IP d1 d2 d3 d4) =
  let digits = map show [d1, d2, d3, d4]
  in pack . concat $ intersperse "."  digits
