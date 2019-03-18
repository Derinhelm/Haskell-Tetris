{-# LANGUAGE ScopedTypeVariables #-}
module Draw where
import Type
import Graphics.Gloss

window :: Display
window = InWindow "Tetris" (1000, 1000) (0, 0)

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

drawField :: Field -> IO()
drawField f = do 
    display window blue (pictures (pictureField  f False))--сильно подумать над сеткой
    display window blue (pictures (pictureField  f True))

--drawNextFigure :: NumberFigure -> IO()
