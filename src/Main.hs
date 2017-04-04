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
parseBirds xx = map read $ words xx

getMostCommonBird bb = snd themax
    where count = birdCount 1 (head bb) (tail bb)
          themax = foldl1 (\(na, a) (nb, b) -> if na>=nb then (na, a) else (nb, b)) count

birdCount::Int -> Int -> [Int] -> [(Int,Int)]
birdCount acc bird [] = (acc, bird) : []
birdCount acc bird (newbird:bb) =
  if bird == newbird
    then birdCount (acc + 1) bird bb
    else (acc, bird) : birdCount 1 newbird bb
