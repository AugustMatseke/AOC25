-- Read input, pattern match on first Letter, then adjust results mod 100
module Day1 where

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let turns = lines contents
  let dial = 50 :: Int
  putStrLn $ "Part 1: " ++ show (findPass dial turns)
  putStrLn $ "Part 2: " ++ show (findZPass dial turns)

parseTurn :: String -> Int
parseTurn [] = 0
parseTurn (x : xs) = case x of
  'L' -> -(read xs)
  'R' -> read xs

findPass :: Int -> [String] -> Int -- dial, turns -> pass
findPass _ [] = 0
findPass dial (t : ts) =
  case newDial of
    0 -> 1 + findPass newDial ts
    _ -> findPass newDial ts
  where
    newDial = (dial + parseTurn t) `mod` 100

findZPass :: Int -> [String] -> Int -- dial, turns -> pass
findZPass _ [] = 0
findZPass dial (t : ts) =
  let turn = parseTurn t
      hits = countZeros dial turn
      newDial = (dial + turn) `mod` 100
   in hits + findZPass newDial ts

countZeros :: Int -> Int -> Int
countZeros _ 0 = 0
countZeros start turn =
  if turn > 0
    then q `div` 100 - p `div` 100
    else (p - 1) `div` 100 - (q - 1) `div` 100
  where
    p = start
    q = start + turn