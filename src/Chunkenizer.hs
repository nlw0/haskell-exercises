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
  (blockSize: fnArg) <- getArgs
  let filename = listToMaybe fnArg
  interactFile filename (f (read blockSize))

interactFile :: Maybe String -> (String->String) -> IO ()
interactFile maybeFilename fun = do
  s <- fromMaybe getContents maybeFile
  putStr $ fun s
  where
    maybeFile = fmap readFile maybeFilename

f blockSize s = unlines $ fmap show $ chunkenize blockSize s

chunkenize :: Int -> String -> [String]
chunkenize baseSize s = addLineToFirstChunk firstLine $ chunkenize' baseSize s
  where (firstLine, _) = takeLine s

chunkenize' :: Int -> String -> [String]
chunkenize' baseSize s = if newChunk /= "" then newChunk : chunkenize' baseSize remaining else []
  where
    (headerToDrop, newData) = takeLine s
    adjustedChunkSize = baseSize - (length headerToDrop)
    (chunkMeat, remaining) = splitAt adjustedChunkSize newData
    (lastline, _) = takeLine remaining
    newChunk = (chunkMeat ++ lastline)

takeLine :: String -> (String, String)
takeLine s = (line ++ lineEnd, tailRest)
  where
    (line, rest) = break (== '\n') s
    (lineEnd, tailRest) = splitAt 1 rest

addLineToFirstChunk :: String -> [String] -> [String]
addLineToFirstChunk a [] = [a]
addLineToFirstChunk a (x:xs) = (a ++ x) : xs
