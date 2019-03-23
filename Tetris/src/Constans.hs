module Constans where
import Graphics.Gloss

-- Random numbers range.
range :: (Int, Int)
range = (1, 7)

fps :: Int
fps = 10

window :: Display
window = InWindow "Tetris" (1000, 1000) (0, 0)

createListFigures :: [Int]
createListFigures = [1, 2, 5, 4, 6, 2, 3, 7, 2, 4, 3]

colorBoard :: Color
colorBoard = white
