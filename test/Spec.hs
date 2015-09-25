module Main where

import           Iptf
import           Iptf.Hosts_Test (tests)
import           Test.Tasty


main :: IO ()
main = defaultMain tests
