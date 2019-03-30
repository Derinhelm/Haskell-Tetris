{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Draw where
import Type
import Graphics.Gloss
import Constans
import System.IO
import System.Random
import Debug.Trace




objects :: Float -> Float -> Color-> Picture 
objects x y col = translate (50 * (y - 6)) (50 * (- x + 6)) $ color col$ rectangleSolid 50 50

drawCell :: Cell -> Picture
drawCell (Cell( numLine :: Int) (numCell :: Int) (cellType :: Int) (cellColor :: Color))
    |(cellType == 2) = (objects (realToFrac numLine) (realToFrac numCell) colorBoard)
    | otherwise = (objects (realToFrac numLine) (realToFrac numCell) cellColor)

drawLine :: Line -> [Picture]
drawLine [] = []
drawLine (x : xs) = (drawCell x : drawLine xs)

pictureField :: Field -> [Picture]
pictureField [] = []
pictureField (x : xs)  = {-trace (show x)-} ((drawLine x) ++ (pictureField xs ))


drawCellForFigure :: [CoordCell] -> Color -> [Picture] ->  [Picture]
drawCellForFigure [] col res = res
drawCellForFigure ((x, y) : xs) col res = drawCellForFigure xs col ([newPic] ++ res)
    where newPic = (translate (-600 + (realToFrac y) * 30) (400 + (realToFrac (-x)) * 30) $ color col $ rectangleSolid 30 30)

drawFigure :: NumberFigure -> [Picture]
drawFigure num = drawCellForFigure coords col []
    where   col = (!!) createColorFigures num
            coords = (!!) createCoordFigures num

drawResult :: Int -> Bool -> [Picture]
drawResult res isEnd | isEnd = [(translate 330 200 $ color yellow $ (Text ((show res)))),
                                    (translate (-250) 370 $ color orange $ (Text "GAME")), 
                                    (translate (100) (370) $ color orange $ (Text "OVER")),
                                    (translate (210) (0) $ color orange $ (Text "Enter")),
                                    (translate (300) (-100) $ color orange $ (Text "=")), 
                                    (translate (250) (-200) $ color orange $ (Text "new")),
                                    (translate (190) (-300) $ color orange $ (Text "game"))
                                    ]
drawResult res isEnd | otherwise = [translate 330 200 $ color yellow $ (Text ((show res)))]
--drawResult res = [translate 330 200 $ color yellow $ (Text (show res)), translate 330 250 $ color yellow $ (Text (show "result"))]

drawGame :: GameState -> Picture
drawGame game@GameState{..} = 
        pictures((pictureField gameField ) ++ (drawFigure (head gameFigures)) ++ (drawResult gameResult endGame))

  

        