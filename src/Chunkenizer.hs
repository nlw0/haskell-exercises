module Chunkenizer where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe
import           System.Environment
import           System.IO

testChunkenizer :: IO ()
testChunkenizer = do
  args <- getArgs
  let filename = listToMaybe args
  interactFile filename f

interactFile :: Maybe String -> (String->String) -> IO ()
interactFile maybeFilename fun = do
  s <- fromMaybe getContents maybeFile
  putStr $ fun s
  where
    maybeFile = fmap readFile maybeFilename

f s = unlines $ fmap show $ startChunkenize 5 s

startChunkenize :: Int -> String -> [String]
startChunkenize n s = addRowToFirstChunk firstRow $ chunkenize n s
  where (firstRow, _) = takeRow s

chunkenize :: Int -> String -> [String]
chunkenize _ "" = []
chunkenize 1 s = [meat]
  where (_, meat) = takeRow s
chunkenize n s = (cleanMeat++bone) : chunkenize (n - 1) remaining
  where baseSize = (div (length s) n)
        (chunkMeat, remaining) = splitAt baseSize s
        (_, cleanMeat) = takeRow chunkMeat
        (bone, _) = takeRow remaining

takeRow s = case span (/= '\n') s of
            (fat, '\n' : cleanMeat) -> (fat ++ "\n", cleanMeat)
            (fat, cleanMeat) -> (fat, cleanMeat)
addRowToFirstChunk :: String -> [String] -> [String]
addRowToFirstChunk a [] = [a]
addRowToFirstChunk a (x:xs) = (a ++ x) : xs
