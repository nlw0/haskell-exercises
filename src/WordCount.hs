module WordCount where

import Control.Applicative
import Control.Monad
import System.IO
import System.Environment
import Data.List
import Data.Maybe

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

f x = let theIn = lines x
          theLines = fmap analyze theIn
      in unlines theLines
      where analyze s = s ++ ", " ++ (show $ length s)

















