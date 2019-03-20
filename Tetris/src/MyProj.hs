{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}


module MyProj    where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Draw
import Type
import Constans

createNextFigure::Int -- получение номера новой следующей фигуры - пока не реализовано
createNextFigure = 1 

shiftFigure :: GameState -> GameState --сдвиг фигуры(на 1 вниз + по клавишам) - пока не реализовано
shiftFigure x  = x

addFigure :: GameState  -> GameState -- падение новой фигуры  - пока не реализовано
addFigure field = field--do


        
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

endGame ::GameState -> GameState
endGame x = x --поменять!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! должно быть без параметров 
            

gameLoop :: Float -> GameState -> GameState
gameLoop _ game@GameState {..}  = 
        if (haveFlyFigure gameField)
                        then (shiftFigure game)
                        else if (checkEnd gameField)
                                then  (endGame game)
                                else  (addFigure game)       
      

--gameLoop :: Float -> Field -> Field --игровой цикл 
--gameLoop _ field  = if haveFlyFigure field 
  --                      then return (shiftFigure field)
   --                     else if checkEnd field
     --                           then return endGame
       --                         else (addFigure field)       



