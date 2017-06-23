module Quicksort where

import Control.Applicative
import Control.Monad
import System.IO
import System.Environment
import Data.List
import Data.Char
import Data.Maybe
import Data.Foldable
import qualified Data.Map as Map

quicksort :: IO ()
quicksort = do
  args <- getArgs
  let filename = listToMaybe args
  interactFile filename f

interactFile :: Maybe String -> (String->String) -> IO ()
interactFile maybeFilename fun = do
    s <- fromMaybe getContents maybeFile
    putStr $ fun s
    where maybeFile = fmap readFile maybeFilename

f x = unlines $ fmap mysort $ lines x

mysort :: String -> String
mysort [] = []
mysort (pivot:xx) = (mysort la) ++ [pivot] ++ (mysort lb)
  where (la, lb) = partition (<= pivot) xx



