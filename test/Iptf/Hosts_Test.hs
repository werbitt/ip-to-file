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
import           Iptf.Hosts.IO         (header, hostsFileToText, hostsToText)
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

instance Arbitrary HostsFile where
  arbitrary = HostsFile <$> arbitrary <*> arbitrary <*> arbitrary

unwrap :: Modifiable a -> a
unwrap (Same x)    = x
unwrap (Changed x) = x

prop_pre_unchanged :: HostsFile -> Hostname -> IP -> Bool
prop_pre_unchanged hf name ip = pre hf == pre (unwrap $ updateHostsFile hf ip name)


prop_post_unchanged :: HostsFile -> Hostname -> IP -> Bool
prop_post_unchanged hf name ip = post hf == post (unwrap $ updateHostsFile hf ip name)
  where
    types = (hf, name, ip) :: (HostsFile, Hostname, IP)

prop_idempotent_add :: HostsFile -> Hostname -> IP -> Bool
prop_idempotent_add hf name ip = hf' == hf''
  where
    hf' = unwrap $ updateHostsFile hf ip name
    hf'' = unwrap $ updateHostsFile hf' ip name

prop_no_header_for_empty_content :: Text -> Text -> Bool
prop_no_header_for_empty_content pre' post' = filter (== header) (lines $ hostsFileToText hf) == []
  where
    hf = HostsFile pre' empty post'

prop_header_for_content :: HostsFile -> Property
prop_header_for_content hf = withHosts hf ==> length headerLines == 1
  where
    headerLines = filter (== header) (lines $ hostsFileToText hf)
    withhosts h = not . null $ hosts h
