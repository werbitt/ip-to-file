module Main where

import           Control.DeepSeq     (NFData, rnf)
import           Criterion.Main
import           Criterion.Types     (Config (..))
import qualified Data.Map.Strict     as M
import qualified Data.Set            as S
import           Iptf.Arbitrary
import           Iptf.Hosts.Internal (Hostname (..), Hosts (..), Record (..),
                                      fromList, getIP, getIP')
import           Iptf.Ip.Internal    (IP (..))
import           Test.QuickCheck     (Gen, arbitrary, elements, generate,
                                      vector, vectorOf)

instance NFData Hosts where
  rnf (Hosts a) = rnf a

instance NFData Hostname where
  rnf (Hostname n) = rnf n

instance NFData IP where
  rnf (IP a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d

data Pos = First | Last

setupEnv :: Int -> Int -> Pos -> IO (Hostname, Hosts)
setupEnv ips names pos = do
  hosts@(Hosts m) <- makeHosts ips names
  let name = case pos of
        First -> head . S.toList . S.unions . M.elems $ m
        Last  -> last . S.toList . S.unions . M.elems $ m
  return $ (name, hosts)

makeHosts :: Int -> Int -> IO Hosts
makeHosts numIPs namesPerIP = do
  ips <- generate $ vectorOf numIPs arbitrary
  ns <- generate $ vectorOf numIPs (vectorOf namesPerIP arbitrary)
  return $ fromList (zipWith Record ips ns)


main :: IO ()
main = defaultMainWith
       (defaultConfig {reportFile = Just "benchmark/bench-getIP.html"})
       [
         env (setupEnv 50 10 Last)
         (\ ~(name, hosts) ->
            bgroup
            "getIP vs getIP' - Large Hosts, name at end"
            [
              bench "getIP" $ nf (getIP name) hosts
            , bench "getIP'" $ nf (getIP' name) hosts
            ])
       , env (setupEnv 50 10 First)
         (\ ~(name, hosts) ->
            bgroup
            "getIP vs getIP' - Large Hosts, name at front"
            [
              bench "getIP" $ nf (getIP name) hosts
            , bench "getIP'" $ nf (getIP' name) hosts
            ])]
