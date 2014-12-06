{-# LANGUAGE OverloadedStrings #-}

module Iptf.Hosts where

import           Control.Applicative
import           Data.Attoparsec.Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Text.IO (readFile)
import           Prelude hiding (readFile, takeWhile)

type Hosts       = Map.Map Ip (S.Set Hostname)
newtype Ip       = Ip T.Text deriving (Show, Eq, Ord)
newtype Hostname = Hostname T.Text deriving (Show, Eq, Ord)
data HostsRecord = HostsRecord Ip [Hostname] deriving (Show)


readHosts :: FilePath -> IO (Either String Hosts)
readHosts p = readFile p >>= return . feedParser hostsParser

fromList :: [HostsRecord] -> Hosts
fromList [] = Map.empty
fromList ((HostsRecord i hs):xs) = Map.union (Map.singleton i (S.fromList hs)) $ fromList xs

ipParser :: Parser Ip
ipParser = takeTill isHorizontalSpace >>= \i -> return $ Ip i

skipHorizontalSpace :: Parser ()
skipHorizontalSpace = skipMany (satisfy isHorizontalSpace)

hostnameParser :: Parser Hostname
hostnameParser = takeTill (\c -> isHorizontalSpace c || isEndOfLine c)
                 >>= \h -> return $ Hostname h

hostnamesParser :: Parser [Hostname]
hostnamesParser = manyTill (hostnameParser <* skipHorizontalSpace) endOfLine

recordParser :: Parser HostsRecord
recordParser = do
  ip' <- ipParser
  skipHorizontalSpace
  hs <- hostnamesParser
  return $ HostsRecord ip' hs

feedEmpty :: Result r -> Result r
feedEmpty = flip feed T.empty

hostsParser :: Parser Hosts
hostsParser = manyTill recordParser endOfInput >>= return . fromList

feedParser :: Parser a -> T.Text -> Either String a
feedParser p t = eitherResult . feedEmpty $ parse p t
