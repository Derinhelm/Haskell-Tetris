{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Type   where
import Graphics.Gloss
import Constans

import System.IO
import System.Random
import Data.Maybe
import Debug.Trace

data TypeCell = Land | Fly | Emp deriving (Show, Eq)

data Cell = Cell {       numLine :: Int
                         , numCell :: Int --номер клетки в столбце
                         , cellType :: TypeCell -- тип клетки 0 - занята(нижние клетки), 1 - занята(летящая клетка), 2 - свободна
                         , cellColor :: Color
                         } deriving Show
type Line = [Cell]
type Field = [Line] 



createLine :: Int -> Line
createLine x = [Cell x y Emp white | y <- [0..9]]

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
createColorFigures = [green, (light (light blue)), violet, red, blue, yellow, greyN 0.5]


createRotateModels :: [(CoordFigures, CoordCell)]
createRotateModels =    [([(0, 0), (0, 1), (1, 0), (1, 1)], (0, 0))

                        , ([(0, 0), (0, 1), (1, 1), (1, 2)], (0, 1))
                        , ([(0, 1), (1, 0), (1, 1), (2, 0)], (1, 1))
                        , ([(0, 0), (0, 1), (1, 1), (1, 2)], (1, 1))
                        , ([(0, 1), (1, 0), (1, 1), (2, 0)], (1, 0))

                        , ([(0, 1), (0, 2), (1, 0), (1, 1)], (1, 1))
                        , ([(0, 0), (1, 0), (1, 1), (2, 1)], (1, 0))
                        , ([(0, 1), (0, 2), (1, 0), (1, 1)], (0, 1))
                        , ([(0, 0), (1, 0), (1, 1), (2, 1)], (1, 1))


                        , ([(0, 0), (0, 1), (0, 2), (1, 1)], (0, 1))
                        , ([(0, 1), (1, 0), (1, 1), (2, 1)], (1, 1))
                        , ([(0, 1), (1, 0), (1, 1), (1, 2)], (1, 1))
                        , ([(0, 0), (1, 0), (1, 1), (2, 0)], (1, 0))

                        , ([(0, 0), (1, 0), (2, 0), (3, 0)], (2, 0))
                        , ([(0, 0), (0, 1), (0, 2), (0, 3)], (0, 1))
                        , ([(0, 0), (1, 0), (2, 0), (3, 0)], (1, 0))
                        , ([(0, 0), (0, 1), (0, 2), (0, 3)], (0, 2))

                        , ([(0, 1), (1, 1), (2, 0), (2, 1)], (2, 1))
                        , ([(0, 0), (1, 0), (1, 1), (1, 2)], (1, 0))
                        , ([(0, 0), (0, 1), (1, 0), (2, 0)], (0, 0))
                        , ([(0, 0), (0, 1), (0, 2), (1, 2)], (0, 2))

                        , ([(0, 0), (1, 0), (2, 0), (2, 1)], (2, 0))
                        , ([(0, 0), (0, 1), (0, 2), (1, 0)], (0, 0))
                        , ([(0, 0), (0, 1), (1, 1), (2, 1)], (0, 1))
                        , ([(0, 2), (1, 0), (1, 1), (1, 2)], (1, 0))
                        ]

data End = Game | FirstEnd | OldEnd deriving (Show, Eq)
 --Game - игра еще идет, FirstEnd - игра только закончилась(надо писать в файл)
--OldEnd - игра давно закончилась


data GameState = GameState
    { userName :: String
    , gameField :: Field    -- поле
    , gameRandomGen :: StdGen -- Random number generator.
    , gameFigures :: [NumberFigure] --бесконечный список из следующих фигур
    , gameResult :: Int -- текущий результат игры
    , coordTetr :: [CoordFigures] -- Tetr - тетрамино
    , colorTetr :: [Color]
    , endGame :: End
    , rotateTypeFigure :: Int -- тип вращения, число от 0 до 27
    , numLoop :: Int -- количество прошедших игровых циклов
    } deriving Show
    

getCellLine :: Cell -> Int
getCellLine c@Cell{..} = numLine



getCellColor :: Cell -> Color
getCellColor c@Cell{..} = cellColor

getCell :: Field -> Int -> Int -> Cell -- получение клетки из поля
getCell field x y = (!!) ((!!) field  x)  y 

getCellType :: Cell -> TypeCell --получение типа клетки 
getCellType cell@Cell{..} = cellType

typeCellFromField :: Field -> Int -> Int -> TypeCell --получение типа клетки по координате клетки
typeCellFromField field x y = getCellType (getCell field x y)


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

findCellCondLine :: Line -> (Cell -> Bool) -> [Cell] -> [Cell]
findCellCondLine [] f goodCells = goodCells 
findCellCondLine (x : xs) f goodCells   | (f x) = findCellCondLine xs f (goodCells ++ [x])
                                        | otherwise = findCellCondLine xs f goodCells

findCellCond :: Field -> (Cell -> Bool) -> [Cell] -> [Cell]
findCellCond [] f goodCells = goodCells
findCellCond (x : xs) f goodCells = findCellCond xs f (goodCells ++ (findCellCondLine x f [])) 


higherCell :: Cell -> Field -> Maybe Cell --клетка на 1 выше данной 
higherCell c@Cell{..} field 
    | (numLine == 0) = Nothing
    | otherwise = (Just (getCell field (numLine - 1) numCell))

lowerCell :: Cell -> Field -> Maybe Cell --клетка на 1 ниже данной 
lowerCell c@Cell{..} field 
    | (numLine == 14) = Nothing
    | otherwise = Just (getCell field (numLine + 1) numCell)


leftCell :: Cell -> Field -> Maybe Cell --клетка на 1 левее данной 
leftCell c@Cell{..} field 
    | (numCell == 0) = Nothing
    | otherwise = Just (getCell field (numLine) (numCell - 1))


rightCell :: Cell -> Field -> Maybe Cell --клетка на 1 правее данной 
rightCell c@Cell{..} field 
    | (numCell == 9) = Nothing
    | otherwise = Just (getCell field numLine (numCell + 1))



changeCellInLine :: Line -> Int -> Cell -> Line 
changeCellInLine line y new = beg ++ [new] ++ end
    where (beg, end1) = (splitAt y line )
          (beg2, end) = (splitAt 1 end1)


changeCellInField :: Field -> Int -> Int -> Cell -> Field
changeCellInField field x y new = result
    where (beg, end1) = splitAt x field 
          (beg2, end) = splitAt 1 end1
          result = beg ++ [(changeCellInLine (head beg2) y new)] ++ end

changeCell :: Field -> Cell -> Cell -> Field
changeCell field oldCell@Cell{..} newCell = changeCellInField field numLine numCell newCell 

