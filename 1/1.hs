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

-- if( bb.ix <= p.x && p.x <= bb.ax && bb.iy <= p.y && p.y <= bb.ay ) {
    -- // Point is in bounding box
-- }
-- bb is the bounding box, (ix,iy) are its top-left coordinates, and (ax,ay) its bottom-right coordinates.  p is the point and (x,y) its coordinates.
-- https://stackoverflow.com/a/18295844
--
insideRect :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
insideRect (x, y) (ix, iy) (ax, ay) = ix <= x && x <= ax && iy <= y && y <= ay

applyInstruction :: Grid -> Instruction -> Grid
applyInstruction g (On, a, b) = map (\(x, y, s) -> if insideRect (x, y) a b then (x,y,True) else (x,y,s)) g
applyInstruction g (Off, a, b) = map (\(x, y, s) -> if insideRect (x, y) a b then (x,y,False) else (x,y,s)) g
applyInstruction g (Toggle, a, b) = map (\(x, y, s) -> if insideRect (x, y) a b then (x,y, not s) else (x,y,s)) g

solve :: [Instruction] -> Int
solve = length . filter (\(_,_,s) -> s == True) . foldl applyInstruction grid

main = do
  content <- readFile "input.txt"
  let input = map strip . lines $ content
  putStrLn $ show . solve . map instruction $ input
