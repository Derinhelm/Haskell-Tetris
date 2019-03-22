{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Type   where
import Graphics.Gloss
import Constans

import System.IO
import System.Random
import Data.Maybe

data Cell = Cell {       numLine :: Int
                         , numCell :: Int --номер клетки в столбце
                         , cellType :: Int -- тип клетки 0 - занята(нижние клетки), 1 - занята(летящая клетка), 2 - свободна
                         --3 ошибка при построении(если уже некуда лететь)
                         , cellColor :: Color
                         } deriving Show
type Line = [Cell]
type Field = [Line] 



createLine :: Int -> Line
createLine x = [Cell x y 0 white | y <- [1..10]]

createField :: Field
createField = [createLine x| x <- [1.. 15]]

type NumberFigure = Int
type CoordCell = (Int, Int)--координаты клетки
type Result = Int
type CoordFigures = [CoordCell]

createCoordFigures :: [CoordFigures]
createCoordFigures = [[(1, 5), (1, 6), (2, 5), (2, 6)]
                    , [(1, 5), (1, 6), (2, 6), (2, 7)]
                    , [(1, 6), (1, 7), (2, 5), (2, 6)]
                    , [(1, 5), (1, 6), (1, 7), (2, 6)]
                    , [(1, 5), (2, 5), (3, 5), (4, 5)]
                    , [(1, 6), (2, 6), (3, 5), (3, 6)]
                    , [(1, 5), (2, 5), (3, 5), (3, 6)]
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
mapField f (x : xs) = ((mapLine f x) : mapField f xs) -----НАДО СДЕЛАТЬ ПРИМЕНИТЬ!!!

higherCell :: Cell -> Field -> Maybe Cell --клетка на 1 выше данной 
higherCell (Cell(numLine :: Int) (numCell :: Int) (cellType :: Int) (cellColor :: Color)) field 
    | (numLine == 1) = Nothing
    | otherwise = Just (getCell field (numLine - 1) numCell)

lowerCell :: Cell -> Field -> Maybe Cell --клетка на 1 ниже данной 
lowerCell (Cell(numLine :: Int) (numCell :: Int) (cellType :: Int) (cellColor :: Color)) field 
    | (numLine == 15) = Nothing
    | otherwise = Just (getCell field (numLine + 1) numCell)


changeCellInLine :: Line -> Int -> Cell -> Line -- протестить!!!!!!!!!!!!!!!
changeCellInLine line y new = beg ++ [new] ++ end
    where (beg, end1) = (splitAt (y - 1) line )
          (beg2, end) = (splitAt 1 end1)


changeCellInField :: Field -> Int -> Int -> Cell -> Field
changeCellInField field x y new = beg ++ [(changeCellInLine (head beg2) y new)] ++ end 
    where (beg, end1) = splitAt (x - 1) field 
          (beg2, end) = splitAt 1 end1

changeCell :: Field -> Cell -> Cell -> Field
changeCell field oldCell@Cell{..} newCell = changeCellInField field numLine numCell newCell 

