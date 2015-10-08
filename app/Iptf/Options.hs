module Iptf.Options where

import           Control.Applicative
import           Data.Maybe          (fromJust)
import qualified Data.Text           as T
import           Iptf.Hosts          (Hostname, hostname)
import           Options.Applicative (Parser, ParserInfo, execParser, help,
                                      helper, info, long, progDesc, short,
                                      strOption, (<>))

data Options = Options { path :: String
                       , name :: Hostname
                       , url  :: String
                       } deriving (Show)

parseOptions :: Parser Options
parseOptions = Options <$> parsePath <*> parseHostname <*> parseUrl

parsePath :: Parser String
parsePath = strOption $
            short 'f' <>
            long "filepath" <>
            help "Filepath to save your IP to"

parseHostname :: Parser Hostname
parseHostname = fmap (fromJust . hostname . T.pack) $ strOption $
                short 'n' <>
                long "name" <>
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
