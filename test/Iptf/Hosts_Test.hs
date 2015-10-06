{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Iptf.Hosts_Test where

import           Control.Applicative   ((<$>), (<*>))
import qualified Data.Map.Strict       as Map
import           Data.Maybe            (fromJust)
import           Data.Monoid           ((<>))
import           Data.Text             (Text, lines, pack)
import           Iptf.Hosts.Internal
import           Iptf.Hosts.IO         (header, hfcToText, hostsToText)
import           Iptf.Ip.Internal
import           Prelude               hiding (lines, null)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Tests" [
  testGroup "Hosts" [
      testCase "Remove solo host" removeSoloHostTest,
      testCase "Remove hostname" removeHostTest,
      testCase "Remove last hostname" removeLastTest,
      testCase "Hosts to text" hostsToTextTest
      ],
  testGroup "Hosts File Structure" [
      testProperty "Don't change pre" prop_pre_unchanged,
      testProperty "Don't change post" prop_post_unchanged,
      testProperty "No header for empty content" prop_no_header_for_empty_content,
      testProperty "Header for content" prop_header_for_content,
      testProperty "Updating is idempotent" prop_idempotent_add
      ]
  ]


removeSoloHostTest :: Assertion
removeSoloHostTest = assertEqual "Removing last host for an IP"
                     (fromList [Record (IP 1 2 3 4) [Hostname "bar"]])
                     (remove (Hostname "foo")
                      (fromList [Record (IP 1 1 1 1) [Hostname "foo"],
                                 Record (IP 1 2 3 4) [Hostname "bar"]]))


removeHostTest :: Assertion
removeHostTest = assertEqual "Remove a hostname"
                     (fromList [Record (IP 1 2 3 4) [Hostname "bar"]])
                     (remove (Hostname "foo")
                      (fromList [Record (IP 1 2 3 4)
                                 [Hostname "foo", Hostname "bar"]]))

removeLastTest :: Assertion
removeLastTest = assertEqual "Remove last host"
                 empty
                 (remove (Hostname "foo")
                  (fromList [Record (IP 1 2 3 4) [Hostname "foo"]]))


hostsToTextTest :: Assertion
hostsToTextTest = assertEqual
                  "One IP and Host"
                  "1.2.3.4\tfoo\n"
                  (hostsToText $ fromList [Record (IP 1 2 3 4) [Hostname "foo"]])

instance Arbitrary Hostname where
  arbitrary = do
    name <- resize 10 $ listOf1 (choose ('a', 'z'))
    return $ fromJust $ hostname (pack name <> ".global")

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

unwrap :: Modifiable a -> a
unwrap (Same x)    = x
unwrap (Changed x) = x

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

prop_no_header_for_empty_content :: Text -> Text -> Bool
prop_no_header_for_empty_content pre' post' = filter (== header) (lines $ hfcToText hfc) == []
  where
    hfc = HostsFileContents pre' empty post'

prop_header_for_content :: HostsFileContents -> Property
prop_header_for_content hfc = withContents hfc ==> length headerLines == 1
  where
    headerLines = filter (== header) (lines $ hfcToText hfc)
    withContents h = not . null $ content h
