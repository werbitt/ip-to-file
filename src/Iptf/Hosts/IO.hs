{-# LANGUAGE OverloadedStrings #-}

module Iptf.Hosts.IO where

import           Control.Applicative  ((<*))
import           Control.Exception    (IOException, catch)
import           Control.Monad        (liftM)
import           Data.Attoparsec.Text (Parser, Result, anyChar, eitherResult,
                                       endOfInput, endOfLine, feed, isEndOfLine,
                                       isHorizontalSpace, manyTill, parse,
                                       satisfy, skipMany, string, takeText,
                                       takeTill)
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Text.IO         (readFile, writeFile)
import           Iptf.Hosts.Internal  (Hostname (..), Hosts, HostsFile (..),
                                       Record (..), fromList, null, toList)
import           Iptf.Ip              (toText)
import           Iptf.Ip.Internal     (parseIP)
import           Prelude              hiding (null, readFile, writeFile)

readHosts :: FilePath -> IO (Either String HostsFile)
readHosts p = catch (liftM (feedParser hostsFileParser) (readFile p)) handler
  where handler :: IOException -> IO (Either String HostsFile)
        handler ex  = return $ Left $ show ex

writeHosts :: FilePath -> HostsFile -> IO ()
writeHosts p = writeFile p . hostsFileToText

safeLast :: Text -> Maybe Char
safeLast t
  | T.null t = Nothing
  | otherwise = Just (T.last t)

(<<>>) :: Text -> Text -> Text
(<<>>) t1 t2 = case safeLast t1 of
  Nothing -> t1 <> t2
  Just '\n' -> t1 <> t2
  _ -> t1 <> "\n" <> t2

hostsFileToText :: HostsFile -> Text
hostsFileToText (HostsFile pre' hosts' end')
  | null hosts' = pre' <<>> end'
  | otherwise    = pre' <<>> header <<>>  hostsToText hosts' <<>> footer <<>> end'

hostsToText :: Hosts -> Text
hostsToText hs = T.unlines $ map recordToText (toList hs)

recordToText :: Record -> Text
recordToText (Record ip hs) = T.intercalate (T.singleton '\t') t
  where
    t   = ip' : hs'
    ip' = toText ip
    hs' = map (\(Hostname n) -> n) hs


skipHorizontalSpace :: Parser ()
skipHorizontalSpace = skipMany (satisfy isHorizontalSpace)

hostnameParser :: Parser Hostname
hostnameParser = takeTill (\c -> isHorizontalSpace c || isEndOfLine c)
                 >>= \h -> return $ Hostname h

hostnamesParser :: Parser [Hostname]
hostnamesParser = manyTill (hostnameParser <* skipHorizontalSpace) endOfLine

recordParser :: Parser Record
recordParser = do
  ip' <- parseIP
  skipHorizontalSpace
  hs <- hostnamesParser
  return $ Record ip' hs

feedEmpty :: Result r -> Result r
feedEmpty = flip feed T.empty

hostsParser :: Parser Hosts
hostsParser = liftM fromList $ manyTill recordParser endOfInput

header :: Text
header = "#### Iptf Hostnames - start ####"

footer:: Text
footer = "#### Iptf Hostnames - end   ####"

hostsFileParser :: Parser HostsFile
hostsFileParser = do
  pre' <- manyTill anyChar $ string header
  records <- manyTill recordParser (string footer)
  post' <- takeText
  return $ HostsFile (T.pack pre') (fromList records) post'

feedParser :: Parser a -> Text -> Either String a
feedParser p t = eitherResult . feedEmpty $ parse p t
