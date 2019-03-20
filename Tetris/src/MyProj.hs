{-# LANGUAGE ScopedTypeVariables #-}

module MyProj    where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Draw
import Type
import Constans

createNextFigure::Int -- получение номера новой следующей фигуры - пока не реализовано
createNextFigure = 1 

shiftFigure :: Field -> Field --сдвиг фигуры(на 1 вниз + по клавишам) - пока не реализовано
shiftFigure x  = x

addFigure :: Field  -> IO Field -- падение новой фигуры  - пока не реализовано
addFigure field = return field--do


        
deleteLines :: Field -> Int -> (Field, Int) -- удаление линии со всеми заполненными  - пока не реализовано
deleteLines f r = (f, r)

checkEnd :: Field -> Bool -- проверка, что игра закончена
checkEnd  field | funFieldAny (\c -> (typeCell c) == 3) field = True -- не удалось построить новое поле, значит конец
                | funFieldAny (\c -> ((typeCell c) == 1) && (typeCell (lowerCell c field) == 0) ) field = True
                | otherwise = False


haveFlyFigure :: Field -> Bool -- поменять!!!!!!!
haveFlyFigure field = (funFieldAll (\c -> ((typeCell c) /= 1) || 
        ((typeCell c == 1) && (typeCell (lowerCell c field) /= 0))) field) && (funFieldAny (\c -> (typeCell c) == 1) field)

handle :: Event -> GameState -> GameState
handle key game = game

endGame :: Field
endGame = createField --поменять!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            

gameLoop :: Float -> GameState -> GameState
gameLoop _ a = a

--gameLoop :: Float -> Field -> Field --игровой цикл 
--gameLoop _ field  = if haveFlyFigure field 
  --                      then return (shiftFigure field)
   --                     else if checkEnd field
     --                           then return endGame
       --                         else (addFigure field)       



