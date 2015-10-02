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


tests :: TestTree
tests = testGroup "Tests" [
  testGroup "Hosts" [
      testCase "Hosts to text" hostsToTextTest
      ],
  testGroup "Hosts File Structure" [
      testProperty "Don't change pre" prop_pre_unchanged,
      testProperty "Don't change post" prop_post_unchanged,
      testProperty "Updating is idempotent" prop_idempotent_add
      ]
  ]

hostsToTextTest :: Assertion
hostsToTextTest = assertEqual
                  "One IP and Host"
                  "1.2.3.4\tfoo\n"
                  (hostsToText $ fromList [Record (IP 1 2 3 4) [Hostname "foo"]])

instance Arbitrary Hostname where
  arbitrary = do
    name <- resize 10 $ listOf1 (choose ('a', 'z'))
    return $ fromJust $ mkHostname (pack name <> ".global")

instance Arbitrary IP where
  arbitrary = do
    [a, b, c, d] <- vector 4
    return $  IP a b c d

instance Arbitrary Record where
  arbitrary = Record <$> arbitrary <*> (resize 20 $ listOf arbitrary)

instance Arbitrary Hosts where
  arbitrary = resize 20 $ fromList <$> listOf arbitrary

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

instance Arbitrary HostsFileContents where
  arbitrary = HostsFileContents <$> arbitrary <*> arbitrary <*> arbitrary


prop_pre_unchanged :: HostsFileContents -> Hostname -> IP -> Bool
prop_pre_unchanged hfc name ip = pre hfc == pre (unwrap $ updateHfc hfc ip name)


prop_post_unchanged :: HostsFileContents -> Hostname -> IP -> Bool
prop_post_unchanged hfc name ip = post hfc == post (unwrap $ updateHfc hfc ip name)
  where
    types = (hfc, name, ip) :: (HostsFileContents, Hostname, IP)

prop_idempotent_add :: HostsFileContents -> Hostname -> IP -> Bool
prop_idempotent_add hfc name ip = hfc' == hfc''
  where
    hfc' = unwrap $ updateHfc hfc ip name
    hfc'' = unwrap $ updateHfc hfc' ip name
