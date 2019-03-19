{-# LANGUAGE ScopedTypeVariables #-}

module MyProj    where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
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

addFigure :: Field  -> Field -- падение новой фигуры  - пока не реализовано
addFigure field = field

deleteLines :: Field -> Int -> (Field, Int) -- удаление линии со всеми заполненными  - пока не реализовано
deleteLines f r = (f, r)

checkEnd :: Field -> Bool -- проверка, что игра закончена
checkEnd  field | funFieldAny (\c -> (typeCell c) == 3) field = True -- не удалось построить новое поле, значит конец
                | funFieldAny (\c -> ((typeCell c) == 1) && (typeCell (lowerCell c field) == 0) ) field = True
                | otherwise = False

checkLand :: Field -> (Bool, Field) --проверка, что фигурка приземлилась 
checkLand t = (True, t)

haveFlyFigure :: Field -> Bool -- поменять!!!!!!!
haveFlyFigure _ = True

handle :: Event -> Field -> IO Field
handle key field = return field

endGame :: Field
endGame = createField --поменять!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            
gameLoop :: Float -> Field -> IO Field --игровой цикл 
gameLoop _ field  = if haveFlyFigure field 
                        then return (shiftFigure field)
                        else if checkEnd field
                                then return endGame
                                else return (addFigure field)       



