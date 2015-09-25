{-# LANGUAGE OverloadedStrings #-}

module Iptf.Hosts.Internal where

import           Control.Applicative
import           Control.Exception
import           Data.Attoparsec.Text
import qualified Data.Map.Strict      as Map
import           Data.Monoid          ((<>))
import qualified Data.Set             as S
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Text.IO         (readFile, writeFile)
import           Iptf.Ip.Internal     (IP (..), parseIP, toText)
import           Prelude              hiding (readFile, takeWhile, writeFile)

type Hosts = Map.Map IP (S.Set Hostname)
newtype Hostname = Hostname Text deriving (Show, Eq, Ord)
data Record = Record IP [Hostname] deriving (Show)
data HostsFileContents = HostsFileContents { pre    :: Text
                                          , content :: Hosts
                                          , post    :: Text } deriving (Show, Eq)
data Modifiable a = Same a | Changed a

unwrap :: Modifiable a -> a
unwrap (Same x)    = x
unwrap (Changed x) = x

mkHostname :: Text -> Hostname
mkHostname = Hostname

readHosts :: FilePath -> IO (Either String HostsFileContents)
readHosts p = catch (readFile p >>= return . feedParser hostsFileParser) handler
  where handler :: IOException -> IO (Either String HostsFileContents)
        handler ex  = return $ Left $ show ex

writeHosts :: FilePath -> HostsFileContents -> IO ()
writeHosts p hosts = writeFile p $ hfcToText hosts

hfcToText :: HostsFileContents -> Text
hfcToText (HostsFileContents pre' content' end') =
  pre' <> header <> hostsToText content' <> footer <> end'

hostsToText :: Hosts -> Text
hostsToText hs = T.unlines $ map entryToText (Map.toList hs)

entryToText :: (IP, S.Set Hostname) -> Text
entryToText (ip, hs) = T.intercalate (T.singleton '\t') t
  where
    t   = ip' : hs'
    ip' = toText ip
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

header :: Text
header = "#### Iptf Hostnames - start ####"

footer:: Text
footer = "#### Iptf Hostnames - end   ####"


hostsFileParser :: Parser HostsFileContents
hostsFileParser = do
  pre' <- manyTill anyChar $ string header
  content <- manyTill recordParser (string footer)
  post' <- takeText
  return $ HostsFileContents (T.pack pre') (fromList content) post'

feedParser :: Parser a -> Text -> Either String a
feedParser p t = eitherResult . feedEmpty $ parse p t

ipForHostname :: Hostname -> Hosts -> Maybe IP
ipForHostname n h = go $ Map.toList h
  where
    go [] = Nothing
    go ((k, v):xs) = if S.member n v
                     then Just k
                     else go xs

updateHfc :: HostsFileContents -> IP -> Hostname -> Modifiable HostsFileContents
updateHfc (HostsFileContents pre' content' end') ip name =
  case update content' name ip of
    Changed h -> Changed $ HostsFileContents pre' h end'
    Same _ -> Same $ HostsFileContents pre' content' end'

update :: Hosts -> Hostname -> IP -> Modifiable Hosts
update hs n ip
  | hostnameExists hs n && pure ip == ipForHostname n hs = Same hs
  | hostnameExists hs n                                  =  Changed . add ip n $ remove n hs
  | otherwise                                            =  Changed $ add ip n hs

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
