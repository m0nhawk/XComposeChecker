module Main where

import Data.List(inits,nub)
import qualified Data.ListTrie.Map as M
import Data.Semigroup ((<>))
import Options.Applicative
import Text.ParserCombinators.Parsec
import System.Environment

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
  print $ M.toList $ prefixOverlap m
  putStr "\n\n\n"
  let keys = filter (not . null) $ nub $ concatMap (inits . (init . fst)) (M.toList $ prefixOverlap m)
  print keys
  print $ map (`M.member` m) keys

main :: IO ()
main = process =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Filepath to .XCompose file"
     <> header "XComposeChecker - Check your .XCompose for duplicates and overlaps" )
