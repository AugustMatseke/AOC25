module Day2 where

import Data.List.Split (splitOn)

main :: IO ()
main = do
  content <- readFile "input.txt"
  let ranges = splitOn "," content
  let invalids = map findInvalids ranges
  putStrLn $ "Sum of invalid numbers: " ++ show (sum invalids)

findInvalids :: String -> Int
findInvalids range = do
  sum (map findOne [low .. high])
  where
    [low, high] = map read (splitOn "-" range) :: [Int]

findOne :: Int -> Int
findOne n
  | even len = do
      let (left, right) = splitAt (len `div` 2) (show n)
      if left == right then n else 0
  | otherwise = 0
  where
    len = length (show n)