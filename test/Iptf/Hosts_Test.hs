{-# LANGUAGE OverloadedStrings #-}

module Iptf.Hosts_Test where
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
