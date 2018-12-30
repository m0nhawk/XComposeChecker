module Main where

import Data.Maybe
import Data.List(inits,nub)
import qualified Data.ListTrie.Map as M
import Data.Semigroup ((<>))
import Options.Applicative
import Text.ParserCombinators.Parsec
import System.Environment

import XComposeTypes
import XComposeParser
import XComposeChecker

newtype Options = Options { path :: String }

options :: Options.Applicative.Parser Options
options = Options
        <$> strOption
            (long "filepath" <> short 'f' <> help "Filepath of .XCompose")

process :: Options -> IO ()
process (Options filepath) = do
  s <- readFile filepath
  let m = either (const M.empty) constructTrie (parse file "" s)
  putStrLn $ mshow m
  putStr "\n"
  putStrLn $ mshow $ prefixOverlap m
  putStr "\n"
  putStrLn $ mshow $ M.filterWithKey (\k v -> k `elem` dupKeys m) m
  putStrLn $ duplicates m

main :: IO ()
main = process =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Filepath to .XCompose file"
     <> header "XComposeChecker - Check your .XCompose for duplicates and overlaps" )
