{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Draw where
import Type
import Graphics.Gloss
import Constans
import System.IO
import System.Random
import Debug.Trace




objects :: Float -> Float -> Color -> Bool -> Picture -- если isGrid = True, то рисуем решетку, значит, надо rectangleWire
objects x y col isGrid  | isGrid = translate (50 * (- y + 5)) (50 * (- x + 6)) $ color black $ rectangleWire 50 50
                        | otherwise = translate (50 * (- y + 5)) (50 * (- x + 6)) $ color col$ rectangleSolid 50 50

drawCell :: Cell -> Bool -> Picture
drawCell (Cell( numLine :: Int) (numCell :: Int) (cellType :: Int) (cellColor :: Color)) isGrid 
    |(cellType == 2) = (objects (realToFrac numLine) (realToFrac numCell) colorBoard isGrid)
    |otherwise = (objects (realToFrac numLine) (realToFrac numCell) cellColor isGrid)

drawLine :: Line -> Bool -> [Picture]
drawLine [] _ = []
drawLine (x : xs) isGrid = (drawCell x isGrid : drawLine xs isGrid)

pictureField :: Field -> Bool -> [Picture]
pictureField [] _ = []
pictureField (x : xs) isGrid = {-trace (show x)-} ((drawLine x isGrid) ++ (pictureField xs isGrid))


drawFigure :: NumberFigure -> [Picture]
drawFigure _ = [translate (-400) 250 $ color green $ rectangleSolid 20 20]

drawResult :: Int -> Picture--[Picture]
drawResult res = translate 330 200 $ color yellow $ (Text (show res))
--drawResult res = [translate 330 200 $ color yellow $ (Text (show res)), translate 330 250 $ color yellow $ (Text (show "result"))]

drawGame :: GameState -> Picture
drawGame game@GameState{..} = 
        pictures((pictureField gameField False) ++ (drawFigure (head gameFigures)) ++ [(drawResult gameResult)])

  

        