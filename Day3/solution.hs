import Data.Char (digitToInt)
import Data.List (tails)

type Bank = [Battery]

type Jolt = Int

type Battery = Int

main :: IO ()
main = do
  content <- readFile "input.txt"
  let banksLines = lines content
      banks = map (map digitToInt) banksLines
      total = sum (map findJoltage banks)
  print total

findJoltage :: Bank -> Jolt
findJoltage bank = maximum [10 * a + b | (a : bs) <- tails bank, b <- bs]