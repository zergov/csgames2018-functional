import Data.String.Utils
import Data.List

columns :: [String] -> [String]
columns ws = [ map (!! i) ws | i <- [0..n-1] ]
  where n = length . head $ ws

occurences :: String -> [(Char, Int)]
occurences = map (\g -> (head g, length g)) . group . sort

solve :: [String] -> String
solve = map (fst . head . (sortBy sortf) . occurences) . columns
  where sortf (_, a) (_, b) = compare (-a) (-b)

main = do
  content <- readFile "input.txt"
  let input = map strip . lines $ content
  putStrLn $ show $ solve input
