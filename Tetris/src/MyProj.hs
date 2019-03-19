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
funFieldAll f field = all (funLineAll f) field 

funFieldAny :: (Cell -> Bool) -> Field -> Bool -- проверка, что в поле есть элементы, удовлетворяющие условию
funFieldAny f field = any (funLineAny f) field 

createNextFigure::Int -- получение номера новой следующей фигуры - пока не реализовано
createNextFigure = 1 

shiftFigure :: Field -> Field --сдвиг фигуры(на 1 вниз + по клавишам) - пока не реализовано
shiftFigure x  = x

addFigure :: Field -> NumberFigure -> Field -- падение новой фигуры  - пока не реализовано
addFigure field _ = field

deleteLines :: Field -> Int -> (Field, Int) -- удаление линии со всеми заполненными  - пока не реализовано
deleteLines f r = (f, r)

checkEnd :: Field -> Bool -- проверка, что игра закончена
checkEnd  field | funFieldAny (\c -> (typeCell c) == 3) field = True -- не удалось построить новое поле, значит конец
                | funFieldAny (\c -> ((typeCell c) == 1) && (typeCell (lowerCell c field) == 0) ) field = True
                | otherwise = False

checkLand :: Field -> (Bool, Field) --проверка, что фигурка приземлилась 
checkLand t = (True, t)
            
gameLoop :: Field -> Int -> NumberFigure -> IO Int --игровой цикл 
gameLoop oldField oldResult nextFigure = if isLand 
                                                then if checkEnd newField1 
                                                        then do
                                                                drawField oldField
                                                                return oldResult
                                                        else do
                                                                drawField newField1
                                                                --можно рисовать и oldField,отличаются типом клетки(незаметно при рисовании) 
                                                                drawField newField2
                                                                gameLoop (addFigure newField2 nextFigure) newResult (createNextFigure)
                                                else 
                                                        do
                                                                drawField oldField
                                                                gameLoop (shiftFigure newField1) newResult nextFigure
    where
        (isLand, newField1) = checkLand oldField
        (newField2, newResult) = deleteLines newField1 oldResult


tetris :: IO Int  --функция, которая создает начальную картину игры, запускает первый игровой цикл, возвращает результат игры
tetris = gameLoop  createField 0 createNextFigure