{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Iptf.Arbitrary where

import           Control.Applicative ((<$>), (<*>))
import           Data.Maybe          (fromJust)
import           Data.Monoid         ((<>))
import           Data.Text           (Text, pack)
import           Iptf.Hosts          (Hostname, hostname)
import           Iptf.Hosts.Internal (Hosts, HostsFile (..), Record (..),
                                      fromList)
import           Iptf.Ip.Internal    (IP (..))
import           Test.QuickCheck     (Arbitrary, arbitrary, choose, listOf,
                                      listOf1, resize, vector)

instance Arbitrary Hostname where
  arbitrary = do
    name <- resize 10 $ listOf1 (choose ('a', 'z'))
    return $ fromJust $ hostname (pack name <> ".global")

instance Arbitrary IP where
  arbitrary = do
    [a, b, c, d] <- vector 4
    return $  IP a b c d

instance Arbitrary Record where
  arbitrary = Record <$> arbitrary <*> resize 20 (listOf arbitrary)

instance Arbitrary Hosts where
  arbitrary = resize 20 $ fromList <$> listOf arbitrary

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

instance Arbitrary HostsFile where
  arbitrary = HostsFile <$> arbitrary <*> arbitrary <*> arbitrary
