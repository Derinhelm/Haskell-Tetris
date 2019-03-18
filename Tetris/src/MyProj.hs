{-# LANGUAGE ScopedTypeVariables #-}

module MyProj    where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Draw
import Type

funLineAll :: (Cell -> Bool) -> Line -> Bool -- проверка, что вся строка удовлетворяет какому-то условию
funLineAll f str1 = all f str1 

funLineAny :: (Cell -> Bool) -> Line -> Bool -- проверка, что в строке есть элементы, удовлетворяющие условию
funLineAny f str1 = any f str1 


funFieldAll :: (Cell -> Bool) -> Field -> Bool -- проверка, что все поле удовлетворяет какому-то условию
funFieldAll f field = all f field 

funFieldAny :: (Cell -> Bool) -> Field -> Bool -- проверка, что в поле есть элементы, удовлетворяющие условию
funFieldAny f field = any f field 

createNextFigure::Int -- получение номера новой следующей фигуры - пока не реализовано
createNextFigure = 1 

shiftFigure :: Field -> Field --сдвиг фигуры(на 1 вниз + по клавишам) - пока не реализовано
shiftFigure x checkFlyFigure = x

addFigure :: Field -> NumberFigure -> Field -- падение новой фигуры  - пока не реализовано
addFigure field _ = field

deleteLines :: Field -> Int -> (Field, Int) -- удаление линии со всеми заполненными  - пока не реализовано
deleteLines f r = (f, r)

checkEnd :: Field -> Bool -- проверка, что фигурка не может дальше лететь
checkEnd  field | funFieldAny (\c -> (typeCell c) == 3) field = True -- не удалось построить новое поле, значит конец
                | funFieldAny (\c -> and ((typeCell c) == 1) (typeCell (lower c field) == 0) ) field = True
                | otherwise = False
            
gameLoop :: Field -> Int -> NumberFigure -> IO Int --игровой цикл 
gameLoop oldField oldResult nextFigure | isLand | checkEnd newField1 = do 
                                                        draw oldField
                                                        return oldResult
                                                | otherwise =  do
                                                        draw newField1
                                                        --можно рисовать и oldField,отличаются типом клетки(незаметно при рисовании) 
                                                        draw newField2
                                                        gameLoop (addFigure nextFigure) newResult (createNextFigure)
                                        | otherwise = do
                                            draw oldField
                                            gameLoop (shiftFigure newField1) newResult nextFigure
    where
        (isLand, newField1) = checkLandFigure oldField
        (newField2, newResult) = deleteLines newField1 oldResult


tetris :: IO Int  --функция, которая создает начальную картину игры, запускает первый игровой цикл, возвращает результат игры
tetris = gameLoop  createField 0 createNextFigure