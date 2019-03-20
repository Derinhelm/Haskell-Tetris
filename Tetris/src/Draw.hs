{-# LANGUAGE ScopedTypeVariables #-}
module Draw where
import Type
import Graphics.Gloss
import Constans
import System.IO
import System.Random




objects :: Float -> Float -> Color -> Bool -> Picture -- если isGrid = True, то рисуем решетку, значит, надо rectangleWire
objects x y col isGrid  | isGrid = translate (50 * (y - 6)) (50 * (x - 7)) $ color black $ rectangleWire 50 50
                        | otherwise = translate (50 * (y - 6)) (50 * (x - 7)) $ color col$ rectangleSolid 50 50

drawCell :: Cell -> Bool -> Picture
drawCell (Cell( numLine :: Int) (numCell :: Int) (cellType :: Int) (cellColor :: Color)) isGrid 
    = objects (realToFrac numLine) (realToFrac numCell) cellColor isGrid

drawLine :: Line -> Bool -> [Picture]
drawLine [] _ = []
drawLine (x : xs) isGrid = (drawCell x isGrid : drawLine xs isGrid)

pictureField :: Field -> Bool -> [Picture]
pictureField [] _ = []
pictureField (x : xs) isGrid = (drawLine x isGrid) ++ (pictureField xs isGrid)


drawFigure :: NumberFigure -> Picture
drawFigure _ = rectangleSolid 10 20

drawResult :: Int -> Picture
drawResult _ = rectangleSolid 10 20

drawGame :: GameState -> Picture
drawGame (GameState(gameField :: Field) (gameRandomGen :: StdGen) (gameFigures :: [NumberFigure]) (gameResult :: Int)) = 
    pictures((pictureField gameField False) ++ [(drawFigure (head gameFigures))] ++ [(drawResult gameResult)])

  