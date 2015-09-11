module Main where

import           Iptf
import           Iptf.Hosts_Test
import           Test.HUnit



main :: IO Counts
main = runTestTT $ TestList [testHosts]
