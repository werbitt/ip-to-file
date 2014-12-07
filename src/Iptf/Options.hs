module Iptf.Options where

import Control.Applicative
import Iptf.Hosts (Hostname(..))
import Options.Applicative (
    Parser
  , ParserInfo
  , execParser
  , short
  , long
  , help
  , helper
  , progDesc
  , (<>)
  , info
  , strOption
  )
import qualified Data.Text as T

data Options = Options { path :: String
                       , hostname :: Hostname
                       , url :: String
                       } deriving (Show)

parseOptions :: Parser Options
parseOptions = Options <$> parsePath <*> parseHostname <*> parseUrl

parsePath :: Parser String
parsePath = strOption $
            short 'f' <>
            long "filepath" <>
            help "Filepath to save your IP to"

parseHostname :: Parser Hostname
parseHostname = fmap (Hostname . T.pack) $ strOption $
                short 'h' <>
                long "hostname" <>
                help "Hostname to use"

parseUrl :: Parser String
parseUrl = strOption $
           short 'u' <>
           long "url" <>
           help "Web url to get your IP from"

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

getOptions :: IO Options
getOptions = execParser $ withInfo parseOptions "Write your IP to a file"
