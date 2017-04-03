module Main where

import Control.Applicative
import Control.Monad
import System.IO
import System.Environment
import Data.List

main :: IO ()
main = do
  args <- getArgs
  mapM_ (interactFile (unlines . f . lines)) args

interactFile :: (String->String) -> String -> IO ()
interactFile fun fileName = do
  s <- readFile fileName
  putStr (fun s)

f :: [String] -> [String]
f ll = map (show . getMostCommonBird . parseBirds) $ tail ll

parseBirds :: String -> [Int]
parseBirds xx = map read $ words xx :: [Int]

getMostCommonBird::[Int] -> [Int]
getMostCommonBird bb = sort bb

--countBirds :: [Int] -> [(Int, Int)]
--countBirds bb = head bb



