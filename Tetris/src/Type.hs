{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Type   where
import Graphics.Gloss
import Constans

import System.IO
import System.Random
import Data.Maybe
import Debug.Trace


data Cell = Cell {       numLine :: Int
                         , numCell :: Int --номер клетки в столбце
                         , cellType :: Int -- тип клетки 0 - занята(нижние клетки), 1 - занята(летящая клетка), 2 - свободна
                         --3 ошибка при построении(если уже некуда лететь)
                         , cellColor :: Color
                         } deriving Show
type Line = [Cell]
type Field = [Line] 



createLine :: Int -> Line
createLine x = [Cell x y 2 white | y <- [0..9]]

createField :: Field
createField = [createLine x| x <- [0.. 14]]

type NumberFigure = Int
type CoordCell = (Int, Int)--координаты клетки
type Result = Int
type CoordFigures = [CoordCell]

createCoordFigures :: [CoordFigures]
createCoordFigures = [[(0, 4), (0, 5), (1, 4), (1, 5)]
                    , [(0, 4), (0, 5), (1, 5), (1, 6)]
                    , [(0, 5), (0, 6), (1, 4), (1, 5)]
                    , [(0, 4), (0, 5), (0, 6), (1, 5)]
                    , [(0, 4), (1, 4), (2, 4), (3, 4)]
                    , [(0, 5), (1, 5), (2, 4), (2, 5)]
                    , [(0, 4), (1, 4), (2, 4), (2, 5)]
                    ]

createColorFigures :: [Color]
createColorFigures = [green, (light blue), violet, red, blue, yellow, greyN 0.5]

data GameState = GameState
    { gameField :: Field    -- поле
    , gameRandomGen :: StdGen -- Random number generator.
    , gameFigures :: [NumberFigure] --бесконечный список из следующих фигур
    , gameResult :: Int -- текущий результат игры
    , coordTetr :: [CoordFigures] -- Tetr - тетрамино
    , colorTetr :: [Color]
    , endGame :: Bool
    } deriving Show
    







getCell :: Field -> Int -> Int -> Cell -- получение клетки из поля
getCell field x y = (!!) ((!!) field  x)  y 

typeCell :: Cell -> Int --получение типа клетки 
typeCell (Cell(numLine :: Int) (numCell :: Int) (cellType :: Int) (cellColor :: Color)) = cellType

typeCellFromField :: Field -> Int -> Int -> Int --получение типа клетки по координате клетки
typeCellFromField field x y = typeCell (getCell field x y)


funLineAll :: (Cell -> Bool) -> Line -> Bool -- проверка, что вся строка удовлетворяет какому-то условию
funLineAll f str1 = all f str1 

funLineAny :: (Cell -> Bool) -> Line -> Bool -- проверка, что в строке есть элементы, удовлетворяющие условию
funLineAny f str1 = any f str1 


funFieldAll :: (Cell -> Bool) -> Field -> Bool -- проверка, что все поле удовлетворяет какому-то условию
funFieldAll f field = all (funLineAll f) field 

funFieldAny :: (Cell -> Bool) -> Field -> Bool -- проверка, что в поле есть элементы, удовлетворяющие условию
funFieldAny f field = any (funLineAny f) field 

mapLine :: (Cell -> Cell) -> Line -> Line
mapLine _ [] = []
mapLine f (x : xs) = ((f x) : mapLine f xs)

mapField :: (Cell -> Cell) -> Field -> Field
mapField _ [] = []
mapField f (x : xs) = ((mapLine f x) : mapField f xs)

higherCell :: Cell -> Field -> Maybe Cell --клетка на 1 выше данной 
higherCell c@Cell{..} field 
    | (numLine == 0) = Nothing
    | otherwise = {-trace ((show c) ++ " " ++ (show (Just (getCell field (numLine - 1) numCell))))-} 
            (Just (getCell field (numLine - 1) numCell))

lowerCell :: Cell -> Field -> Maybe Cell --клетка на 1 ниже данной 
lowerCell (Cell(numLine :: Int) (numCell :: Int) (cellType :: Int) (cellColor :: Color)) field 
    | (numLine == 14) = Nothing
    | otherwise = Just (getCell field (numLine + 1) numCell)


changeCellInLine :: Line -> Int -> Cell -> Line -- протестить!!!!!!!!!!!!!!!
changeCellInLine line y new = --(trace ((show beg) ++ (show end)))
                                 beg ++ [new] ++ end
    where (beg, end1) = (splitAt y line )
          (beg2, end) = (splitAt 1 end1)


changeCellInField :: Field -> Int -> Int -> Cell -> Field
changeCellInField field x y new = --(trace (show result)) 
                                    result
    where (beg, end1) = splitAt x field 
          (beg2, end) = splitAt 1 end1
          result = beg ++ [(changeCellInLine (head beg2) y new)] ++ end

changeCell :: Field -> Cell -> Cell -> Field
changeCell field oldCell@Cell{..} newCell = changeCellInField field numLine numCell newCell 

