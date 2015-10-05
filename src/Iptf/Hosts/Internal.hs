{-# LANGUAGE OverloadedStrings #-}

module Iptf.Hosts.Internal where

import           Control.Applicative  (pure, (<*))
import           Control.Exception
import           Control.Monad        (liftM)
import           Data.Attoparsec.Text
import qualified Data.Map.Strict      as Map
import           Data.Monoid          ((<>))
import qualified Data.Set             as S
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Text.IO         (readFile, writeFile)
import           Iptf.Ip.Internal     (IP (..), parseIP, toText)
import           Prelude              hiding (null, readFile, takeWhile,
                                       writeFile)

newtype Hosts = Hosts (Map.Map IP (S.Set Hostname)) deriving (Show, Eq)
newtype Hostname = Hostname Text deriving (Show, Eq, Ord)
data Record = Record IP [Hostname] deriving (Show)
data HostsFileContents = HostsFileContents { pre    :: Text
                                          , content :: Hosts
                                          , post    :: Text } deriving (Show, Eq)
data Modifiable a = Same a | Changed a deriving (Show)

unwrap :: Modifiable a -> a
unwrap (Same x)    = x
unwrap (Changed x) = x

mkHostname :: Text -> Maybe Hostname
mkHostname t
  | t == ""    = Nothing
  | otherwise  = Just $ Hostname t

readHosts :: FilePath -> IO (Either String HostsFileContents)
readHosts p = catch (liftM (feedParser hostsFileParser) (readFile p)) handler
  where handler :: IOException -> IO (Either String HostsFileContents)
        handler ex  = return $ Left $ show ex

writeHosts :: FilePath -> HostsFileContents -> IO ()
writeHosts p hosts = writeFile p $ hfcToText hosts

safeLast :: Text -> Maybe Char
safeLast t
  | T.null t = Nothing
  | otherwise = Just (T.last t)

(<<>>) :: Text -> Text -> Text
(<<>>) t1 t2 = case safeLast t1 of
  Nothing -> t1 <> t2
  Just '\n' -> t1 <> t2
  _ -> t1 <> "\n" <> t2

hfcToText :: HostsFileContents -> Text
hfcToText (HostsFileContents pre' content' end')
  | null content' = pre' <<>> end'
  | otherwise    = pre' <<>> header <<>>  hostsToText content' <<>> footer <<>> end'

hostsToText :: Hosts -> Text
hostsToText hs = T.unlines $ map recordToText (toList hs)

recordToText :: Record -> Text
recordToText (Record ip hs) = T.intercalate (T.singleton '\t') t
  where
    t   = ip' : hs'
    ip' = toText ip
    hs' = map (\(Hostname n) -> n) hs


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
hostsParser = liftM fromList $ manyTill recordParser endOfInput

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
ipForHostname n (Hosts h) = go $ Map.toList h
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
  | hostnameExists hs n                                  =  Changed . add ip [n] $ remove n hs
  | otherwise                                            =  Changed $ add ip [n] hs

empty :: Hosts
empty = Hosts  Map.empty

union :: Hosts -> Hosts -> Hosts
union (Hosts h1) h2 = Hosts $ Map.union h1 h2'
  where
    (Hosts h2') = foldr remove h2 ns1
    ns1         = S.toList . S.unions $ Map.elems h1

add :: IP -> [Hostname] -> Hosts -> Hosts
add ip ns hs = Hosts $ Map.insertWith S.union ip (S.fromList ns) hs'
  where
    (Hosts hs') = foldr remove hs ns

new :: IP -> [Hostname] -> Hosts
new ip ns = Hosts $ Map.singleton ip (S.fromList ns)

remove :: Hostname -> Hosts -> Hosts
remove n (Hosts hs) = Hosts $ Map.mapMaybe prune hs
  where
    prune ns  = maybeSet $ S.delete n ns
    maybeSet s = if S.null s then Nothing else Just s

toList :: Hosts -> [Record]
toList (Hosts m) = map (\(ip, s) -> Record ip (S.toList s)) entries
  where
    entries = Map.toList m

fromList :: [Record] -> Hosts
fromList []               = empty
fromList (Record _ []:xs) = fromList xs
fromList (Record i hs:xs) = new i hs `union` fromList xs

fromList' :: [Record] -> Hosts
fromList' = foldr addRecord empty
  where
    addRecord (Record _ []) = id
    addRecord (Record ip ns) = add ip ns

null :: Hosts -> Bool
null (Hosts h) = Map.null h

hostnameExists :: Hosts -> Hostname -> Bool
hostnameExists (Hosts hs) n = S.member n $ S.unions (Map.elems hs)
