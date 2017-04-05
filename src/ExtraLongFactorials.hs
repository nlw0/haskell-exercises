module ExtraLongFactorials where

import Control.Applicative
import Control.Monad
import System.IO
import System.Environment
import Data.List
import Data.Maybe

extraLongFactorialsMain :: IO ()
extraLongFactorialsMain = do
  args <- getArgs
  let filename = listToMaybe args
  interactFile (show . f . read) $ filename

interactFile :: (String->String) -> Maybe String -> IO ()
interactFile fun filename = do
  s <- fromMaybe getContents $ fmap readFile $ filename
  putStr $ fun s

f :: Integer->Integer
f x = foldl1' (*) [x,x-1..1]

