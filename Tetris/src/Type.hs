{-# LANGUAGE ScopedTypeVariables #-}

module Type   where
import Graphics.Gloss

data Cell = Cell { numLine :: Int
                         , numCell :: Int --номер клетки в столбце
                         , cellType :: Int -- тип клетки 0 - занята(нижние клетки), 1 - занята(летящая клетка), 2 - свободна
                         --3 ошибка при построении(если уже некуда лететь)
                         , cellColor :: Color
                         } deriving Show
type Line = [Cell]
type Field = [Line] 

type NumberFigure = Int

type Indices = [(Int, Int)]

createLine :: Int -> Line
createLine x = [Cell x y 0 white | y <- [1..10]]

createField :: Field
createField = [createLine x| x <- [1.. 15]]

getCell :: Field -> Int -> Int -> Cell -- получение клетки из поля
getCell field x y = (!!) ((!!) field  x)  y

typeCell :: Cell -> Int --получение типа клетки 
typeCell (Cell(numLine :: Int) (numCell :: Int) (cellType :: Int) (cellColor :: Color)) = cellType

typeCellFromField :: Field -> Int -> Int -> Int --получение типа клетки по координате клетки
typeCellFromField field x y = typeCell (getCell field x y)

createIndices :: Indices --создание множества индексов 
createIndices = [(x, y)| x <- [1..10], y <- [1 ..15]]

lowerCell :: Cell -> Field -> Cell --клетка на 1 ниже данной 
lowerCell (Cell(numLine :: Int) (numCell :: Int) (cellType :: Int) (cellColor :: Color)) field 
    = getCell field (numLine + 1) numCell