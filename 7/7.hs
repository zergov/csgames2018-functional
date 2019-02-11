import qualified Data.Set as S

move :: Char -> (Int, Int) -> (Int, Int)
move '^' (x, y) = (x, y + 1)
move '>' (x, y) = (x + 1, y)
move 'v' (x, y) = (x, y - 1)
move '<' (x, y) = (x - 1, y)

start = (0, 0)

moves = S.size . snd . foldl f (start, (S.singleton start))
  where f (pos, seen) d = (move d pos, S.insert (move d pos) seen)

main = do
  path <- readFile "input.txt"
  putStr $ show $ moves path
