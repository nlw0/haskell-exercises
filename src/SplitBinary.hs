module SplitBinary where

import Control.Applicative
import Control.Monad
import System.IO
import System.Environment
import Data.List
import Data.Maybe

splitBinaryMain :: IO ()
splitBinaryMain = do
  args <- getArgs
  let filename = listToMaybe args
  interactFile f $ filename

interactFile :: (String->String) -> Maybe String -> IO ()
interactFile fun filename = do
  s <- fromMaybe getContents $ fmap readFile $ filename
  putStr $ fun s

f :: String->String
f x = show $ gg x

gg::String->[String]
gg "" = []
gg xs = a : gg b
    where (a, b) = g xs

g :: String->(String, String)
g "" = ("", "")
g (xx:xs) = if (head xx == 'a')
    then ("" , ('a':xs))
    else xx : (g xs)

--g :: String->String->[String]
--g acc ('a':xs) = (reverse acc) : (g "a" xs)
--g acc (xx:xs) = g (xx:acc) xs
--g acc [] = [(reverse acc)]
