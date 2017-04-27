module WordCount where

import Control.Applicative
import Control.Monad
import System.IO
import System.Environment
import Data.List
import Data.Char
import Data.Maybe
import Data.Foldable
import qualified Data.Map as Map

wordCount :: IO ()
wordCount = do
  args <- getArgs
  let filename = listToMaybe args
  interactFile filename f

interactFile :: Maybe String -> (String->String) -> IO ()
interactFile maybeFilename fun = do
    s <- fromMaybe getContents maybeFile
    putStr $ fun s
    where maybeFile = fmap readFile maybeFilename

f x = let normalizedWords = fmap normalizeString $ words x
          count = countStuff normalizedWords
          hitsPerWord = sortBy (compare) [(b,a) | (a,b) <- Map.toList count]
          prettyPrint = unlines [ w ++ ": " ++ (show f) | (f, w) <- hitsPerWord]
      in prettyPrint

countStuff :: (Foldable t, Ord a) => t a -> Map.Map a Integer
countStuff stuff = foldl (\d k-> Map.insertWith (+) k 1 d) Map.empty stuff

normalizeString :: String -> String
normalizeString s = map toLower $ filter isLetter s
