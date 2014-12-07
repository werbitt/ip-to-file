{-# LANGUAGE OverloadedStrings #-}

module Iptf.Hosts where

import           Control.Applicative
import           Data.Attoparsec.Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Text.IO (readFile)
import           Prelude hiding (readFile, takeWhile)
import           Iptf.Ip

type Hosts       = Map.Map IP (S.Set Hostname)
newtype Hostname = Hostname T.Text deriving (Show, Eq, Ord)
data Record = Record IP [Hostname] deriving (Show)


readHosts :: FilePath -> IO (Either String Hosts)
readHosts p = readFile p >>= return . feedParser hostsParser

fromList :: [Record] -> Hosts
fromList [] = Map.empty
fromList ((Record i hs):xs) = Map.union (Map.singleton i (S.fromList hs)) $ fromList xs

skipHorizontalSpace :: Parser ()
skipHorizontalSpace = skipMany (satisfy isHorizontalSpace)

hostnameParser :: Parser Hostname
hostnameParser = takeTill (\c -> isHorizontalSpace c || isEndOfLine c)
                 >>= \h -> return $ Hostname h

hostnamesParser :: Parser [Hostname]
hostnamesParser = manyTill (hostnameParser <* skipHorizontalSpace) endOfLine

recordParser :: Parser Record
recordParser = do
  ip' <- parseIP
  skipHorizontalSpace
  hs <- hostnamesParser
  return $ Record ip' hs

feedEmpty :: Result r -> Result r
feedEmpty = flip feed T.empty

hostsParser :: Parser Hosts
hostsParser = manyTill recordParser endOfInput >>= return . fromList

feedParser :: Parser a -> T.Text -> Either String a
feedParser p t = eitherResult . feedEmpty $ parse p t

ipForHostname :: Hosts -> Hostname -> Maybe IP
ipForHostname h n = go $ Map.toList h
  where
    go [] = Nothing
    go ((k, v):xs) = if S.member n v
                     then Just k
                     else go xs
