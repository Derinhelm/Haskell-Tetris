module Constans where
import Graphics.Gloss
import System.Random


-- Random numbers range.
range :: (Int, Int)
range = (0, 6)

fps :: Int
fps = 4


window :: Display
window = InWindow "Tetris" (1100, 1000) (0, 0)


createListFigures :: StdGen -> [Int]
createListFigures gen = randomRs (0, 6) gen

colorBoard :: Color
colorBoard = white
