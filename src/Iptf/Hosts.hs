{-# LANGUAGE OverloadedStrings #-}

module Iptf.Hosts where

import           Control.Applicative
import           Data.Attoparsec.Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.IO (readFile)
import           Prelude hiding (readFile, takeWhile)
import           Iptf.Ip (IP(..), parseIP, showIP)
import           Control.Exception

type Hosts       = Map.Map IP (S.Set Hostname)
newtype Hostname = Hostname Text deriving (Show, Eq, Ord)
data Record = Record IP [Hostname] deriving (Show)

readHosts :: FilePath -> IO (Either String Hosts)
readHosts p = catch (readFile p >>= return . feedParser hostsParser) handler
  where handler :: IOException -> IO (Either String Hosts)
        handler ex  = return $ Left $ show ex

getHosts :: FilePath -> IO Hosts
getHosts p = readHosts p >>= \hosts ->
  case hosts of
   Left _  -> return Map.empty
   Right h -> return h

toText :: Hosts -> Text
toText hs = T.unlines $ map entryToText (Map.toList hs)

entryToText :: (IP, S.Set Hostname) -> Text
entryToText (ip, hs) = T.intercalate (T.singleton '\t') t
  where
    t   = ip' : hs'
    ip' = showIP ip
    hs' = map (\(Hostname n) -> n) $ S.toList hs

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

feedParser :: Parser a -> Text -> Either String a
feedParser p t = eitherResult . feedEmpty $ parse p t

ipForHostname :: Hostname -> Hosts -> Maybe IP
ipForHostname n h = go $ Map.toList h
  where
    go [] = Nothing
    go ((k, v):xs) = if S.member n v
                     then Just k
                     else go xs

update :: Hosts -> Hostname -> IP -> Hosts
update hs n ip
  | hostnameExists hs n && pure ip == ipForHostname n hs = hs
  | hostnameExists hs n                                  =  add ip n $ remove n hs
  | otherwise                                            =  add ip n hs

add :: IP -> Hostname -> Hosts -> Hosts
add ip n hs = Map.insertWith S.union ip (S.singleton n) hs

remove :: Hostname -> Hosts -> Hosts
remove n hs = case ip of
  Just ip' ->
    if S.null $ remainingNames ip'
    then Map.delete ip' hs
    else Map.insert ip' (remainingNames ip') hs
  Nothing -> hs
  where
    ip               = ipForHostname n hs
    remainingNames i = S.delete n $ Map.findWithDefault S.empty i hs


hostnameExists :: Hosts -> Hostname -> Bool
hostnameExists hs n = S.member n $ S.unions (Map.elems hs)
