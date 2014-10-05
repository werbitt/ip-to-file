module Main where

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
  , option
  , auto
  )

import Prelude hiding (FilePath)
--import System.Path (AbsFile)

data Options = Options { path :: String  } deriving (Read, Show)

parseOptions :: Parser Options
parseOptions = Options <$> parsePath

parsePath :: Parser String
parsePath = strOption $
            short 'f' <>
            long "filepath" <>
            help "Filepath to save your IP to"



withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

run :: Options -> IO ()
run opts = putStrLn $ path opts

main :: IO ()
main = run =<< (execParser $ withInfo parseOptions "Write your IP to a file")
