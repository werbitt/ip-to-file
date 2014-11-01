module Main where

import qualified Data.ByteString.Lazy.Char8 as L
import           Iptf.Ip
import           Iptf.Options
import           System.IO

run :: Options -> IO ()
run opts = do
  f <- openFile (path opts) WriteMode
  i <- ip
  L.hPutStrLn f i
  hClose f

main :: IO ()
main = getOptions >>= run
