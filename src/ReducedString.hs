module ReducedString where

import Control.Applicative
import Control.Monad
import System.IO
import System.Environment
import Data.List
import Data.Maybe

reducedStringMain :: IO ()
reducedStringMain = do
  args <- getArgs
  let filename = listToMaybe args
  interactFile (f . head . lines) $ filename

interactFile :: (String->String) -> Maybe String -> IO ()
interactFile fun filename = do
  s <- fromMaybe getContents $ fmap readFile $ filename
  putStr $ fun s

f :: String->String
f x = if reduced == "" then "Empty String" else reduced
    where reduced = h x

h x = if x == iter then x else h $ iter
    where iter = g x

g (x:y:ss) = if x==y then g ss else x : (g (y : ss))
g x = x
