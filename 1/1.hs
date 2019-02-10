import Data.String.Utils
import Data.Maybe
import qualified Data.Map as M

type Light = (Int, Int, Bool)
type Grid = [Light]
type Instruction = (Command, (Int, Int), (Int, Int))

data Command = On | Off | Toggle deriving (Show)

coords :: String -> [(Int, Int)]
coords xs = [(vec $ head coordstr), (vec $ last coordstr)]
  where
    coordstr = drop (length ws - 3) ws
    ws = words xs
    vec coord = read ("(" ++ coord ++ ")")

instruction :: String -> Instruction
instruction ('t':'u':'r':'n':' ':'o':'n':xs) = (On, (head $ coords xs), (last $ coords xs))
instruction ('t':'u':'r':'n':' ':'o':'f':'f':xs) = (Off, (head $ coords xs), (last $ coords xs))
instruction ('t':'o':'g':'g':'l':'e':xs) = (Toggle, (head $ coords xs), (last $ coords xs))

grid :: Grid
grid = [(i, j, False) | i <- [0..1199], j <- [0..1199]]

applyInstruction :: Grid -> Instruction -> Grid
applyInstruction g (On, a, b) = map (\(x, y, s) -> if (x,y) >= a && (x,y) <= b then (x,y,True) else (x,y,s)) g
applyInstruction g (Off, a, b) = map (\(x, y, s) -> if (x,y) >= a && (x,y) <= b then (x,y,False) else (x,y,s)) g
applyInstruction g (Toggle, a, b) = map (\(x, y, s) -> if (x,y) >= a && (x,y) <= b then (x,y, not s) else (x,y,s)) g

solve :: [Instruction] -> Int
solve = length . filter (\(_,_,s) -> s == True) . foldl applyInstruction grid

main = do
  content <- readFile "input.txt"
  let input = map strip . lines $ content
  putStrLn $ show . solve . map instruction $ input
