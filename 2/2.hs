import Data.String.Utils
import Data.List

solve :: [Int] -> Int
solve xs = length [x | x <- subsequences xs, sum x == 150]

main = do
  content <- readFile "input.txt"
  let input = map read . map strip . lines $ content
  putStrLn $ show $ solve input
