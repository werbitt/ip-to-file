module Iptf.Options where

import Control.Applicative
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

data Options = Options { path :: String
                       , url :: String
                       , urlFile :: String} deriving (Show)

parseOptions :: Parser Options
parseOptions = Options <$> parsePath <*> parseUrl <*> parseUrlFile

parsePath :: Parser String
parsePath = strOption $
            short 'f' <>
            long "filepath" <>
            help "Filepath to save your IP to"

parseUrl :: Parser String
parseUrl = strOption $
           short 'u' <>
           long "url" <>
           help "Web url to get your IP from"

parseUrlFile :: Parser String
parseUrlFile = strOption $
               short 'U' <>
               long "url-file" <>
               help "File with a list of urls to use to get your IP"

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

getOptions :: IO Options
getOptions = execParser $ withInfo parseOptions "Write your IP to a file"
