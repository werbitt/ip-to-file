{-# LANGUAGE OverloadedStrings #-}

module Iptf.Hosts.Internal where

import           Control.Applicative (pure)
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as S
import           Data.Text           (Text)
import           Iptf.Ip.Internal    (IP)


-- | Hosts is a map from and IP to a set of Hostnames. A Hostname can
-- only appear once in a Hosts
newtype Hosts = Hosts (Map.Map IP (S.Set Hostname)) deriving (Show, Eq)

-- | A Hostname cannot be blank
newtype Hostname = Hostname Text deriving (Show, Eq, Ord)

-- | A Record consists of an IP and a list of Hostnames. It's basically
-- a reified entry from Hosts
data Record = Record IP [Hostname] deriving (Show)

-- | HostsFile represents an /etc/hosts file. It contains any
-- text before the ip-to-file section. The ip-to-file section represented
-- as a Hosts, and any text after the ip-to-file section.
data HostsFile = HostsFile { pre   :: Text
                           , hosts :: Hosts
                           , post  :: Text } deriving (Show, Eq)

-- | Modifiable is used to tag whether something has changed or not. This
-- is used to prevent unneccessary writes to disk.
data Modifiable a = Same a | Changed a deriving (Show)


-- | Smart constructor for Hostname, doesn't allow blanks.
hostname :: Text -> Maybe Hostname
hostname t
  | t == ""    = Nothing
  | otherwise  = Just $ Hostname t


ipForHostname :: Hostname -> Hosts -> Maybe IP
ipForHostname n (Hosts h) = go $ Map.toList h
  where
    go [] = Nothing
    go ((k, v):xs) = if S.member n v
                     then Just k
                     else go xs

updateHostsFile :: HostsFile -> IP -> Hostname -> Modifiable HostsFile
updateHostsFile (HostsFile pre' hosts' end') ip name =
  case update hosts' name ip of
    Changed h -> Changed $ HostsFile pre' h      end'
    Same _    -> Same    $ HostsFile pre' hosts' end'

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
