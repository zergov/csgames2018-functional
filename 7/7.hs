import qualified Data.Set as S

move :: Char -> (Int, Int) -> (Int, Int)
move '^' (x, y) = (x, y + 1)
move '>' (x, y) = (x + 1, y)
move 'v' (x, y) = (x, y - 1)
move '<' (x, y) = (x - 1, y)

start :: (Int, Int)
start = (0, 0)

easy :: String -> Int
easy = S.size . snd . foldl f (start, (S.singleton start))
  where f (pos, seen) d = (move d pos, S.insert (move d pos) seen)

hard :: String -> Int
hard = S.size . snd . foldl f ((start, start), S.singleton start) . zip [1..]
  where
    f ((pa, pb), seen) (i, d) =
      if odd i then ((move d pa, pb), S.insert (move d pa) seen)
      else ((pa, move d pb), S.insert (move d pb) seen)

main = do
  path <- readFile "input.txt"
  putStrLn $ "easy: " ++ (show $ easy path)
  putStrLn $ "hard: " ++ (show $ hard path)
