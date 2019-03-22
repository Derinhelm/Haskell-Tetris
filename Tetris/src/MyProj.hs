{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}


module MyProj    where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Draw
import Type
import Constans
import System.Random
import Data.Maybe

createCellShiftFigure :: Field -> Cell -> Cell
createCellShiftFigure field c@Cell{..}  = case hc of 
        Nothing -> (Cell numLine numCell 0 colorBoard)
        Just t -> case (typeCell t) of
                        2 -> c 
                        0 -> c
                        1 -> (Cell numLine numCell 1 cellColor)
        where hc = (higherCell c field)

shiftFigureOnField :: Field -> Field
--shiftFigureOnField _ = createField
shiftFigureOnField field = mapField (\c -> (createCellShiftFigure field c)) field

shiftFigure :: GameState -> GameState --сдвиг фигуры(на 1 вниз)
shiftFigure GameState{..}  = (GameState (shiftFigureOnField gameField) gameRandomGen gameFigures gameResult coordTetr colorTetr) 



newFigureOnField :: Field -> CoordFigures -> Color -> Field
newFigureOnField field [] color = field
newFigureOnField field ((x, y): xs) color = newFigureOnField (changeCellInField field x y (Cell x y 1 color)) xs color
--map (\(x, y) -> (changeCellInField field x y (Cell x y 1 color))) coords
--addFigureOnField field numb = map markFigure 

newFigureOnGame :: GameState  -> GameState -- падение новой фигуры 
newFigureOnGame GameState{..} =
        (GameState (newFigureOnField gameField  ((!!) coordTetr nextFigure) ((!!) colorTetr nextFigure))
            gameRandomGen (tail gameFigures) gameResult coordTetr colorTetr )
                where nextFigure = (head gameFigures)
 


        
deleteLines :: Field -> Int -> (Field, Int) -- удаление линии со всеми заполненными  - пока не реализовано
deleteLines f r = (f, r)

checkEnd :: Field -> Bool -- проверка, что игра закончена
checkEnd  field | funFieldAny (\c -> ((typeCell c) == 1) && (typeCell (fromJust (lowerCell c field)) == 0))  field = True
--fromJust - там не может быть maybe, 
                | otherwise = False

checkFlyCell ::  Field -> Cell -> Bool
checkFlyCell field c = case lc of
        Nothing -> True
        Just lc1 -> ((typeCell c) /= 1) || ((typeCell c == 1) && (typeCell lc1 /= 0))
        where lc = lowerCell c field

haveFlyFigure :: Field -> Bool -- поменять!!!!!!!
haveFlyFigure field = (funFieldAll (checkFlyCell field) field) && (funFieldAny (\c -> (typeCell c) == 1) field)
--проверка -    1.что клетка - или не летящая, или летящая, но под ней свободно
--              2.что на поле есть летящие клетки
        
handle :: Event -> GameState -> GameState
handle key game = game

endGame ::GameState -> GameState
endGame x = x --поменять!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! должно быть без параметров 
            

gameLoop :: Float -> GameState -> GameState
gameLoop _ game@GameState {..}  = 
        if (haveFlyFigure gameField)
                        then (shiftFigure game)
                        else if (checkEnd gameField)
                                then  (endGame game)-- !!!!!!!!!!д.б. без параметров
                                else  (newFigureOnGame game)       
      


--gameLoop :: Float -> Field -> Field --игровой цикл 
--gameLoop _ field  = if haveFlyFigure field 
  --                      then return (shiftFigure field)
   --                     else if checkEnd field
     --                           then return endGame
       --                         else (addFigure field)       



