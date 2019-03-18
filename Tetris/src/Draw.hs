{-# LANGUAGE ScopedTypeVariables #-}
module Draw where
import Type
import Graphics.Gloss

window :: Display
window = InWindow "Tetris" (200, 200) (10, 10)

background :: Color
background = white

objects :: Float -> Float -> Picture
objects x y = Rectangle (1 * x) (1 * y)

drawCell :: Cell -> IO()
drawCell (Cell( numLine :: Int) (numCell :: Int) (cellType :: Int) (cellColor :: Color)) 
    = display window cellColor (objects (realToFrac numLine) (realToFrac numCell))

drawLine :: Line -> IO()
drawLine [] = return()
drawLine (x : xs) = do
    drawCell x
    drawLine xs

drawField :: Field -> IO()
drawField [] = return()
drawField (x : xs) = do
    drawLine x
    drawField xs