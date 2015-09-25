{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Iptf.Hosts_Test where

import           Control.Applicative   ((<$>), (<*>))
import           Data.Monoid           ((<>))
import           Data.Text             (Text, pack)
import           Iptf.Hosts.Internal
import           Iptf.Ip.Internal
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

testHosts :: Test
testHosts = TestList [getHostsTest]

getHostsTest :: Test
getHostsTest = TestCase $ assertEqual
               "One IP and Host"
               "1.2.3.4\tfoo\n"
               (hostsToText $ fromList [Record (IP 1 2 3 4) [Hostname "foo"]])
instance Arbitrary Hostname where
  arbitrary = do
    name <- listOf1 (choose ('a', 'z'))
    return $ (mkHostname . pack) (name <> ".global")

instance Arbitrary IP where
  arbitrary = do
    [a, b, c, d] <- vector 4
    return $  IP a b c d

instance Arbitrary Record where
  arbitrary = Record <$> arbitrary <*> listOf arbitrary

instance Arbitrary Hosts where
  arbitrary = fromList <$> listOf arbitrary

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

instance Arbitrary HostsFileContents where
  arbitrary = HostsFileContents <$> arbitrary <*> arbitrary <*> arbitrary

