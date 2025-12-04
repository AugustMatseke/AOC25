-- Read input, pattern match on first Letter, then adjust results mod 100
module Day1 where

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let turns = lines contents
  let dial = 50 :: Int
  putStrLn $ "Part 1: " ++ show (findPass dial turns)

parseTurn :: String -> Int
parseTurn [] = 0
parseTurn (x : xs) = case x of
  'L' -> -(read xs)
  'R' -> read xs

findPass :: Int -> [String] -> Int -- dial, turns -> pass
findPass dial (t : ts) =
  case newDial of
    0 -> 1 + findPass newDial ts
    _ -> findPass newDial ts
  where
    newDial = (dial + parseTurn t) `mod` 100
findPass dial [] = 0