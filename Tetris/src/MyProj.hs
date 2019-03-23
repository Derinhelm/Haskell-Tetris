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
import Debug.Trace
getNewColor :: Cell -> Color
getNewColor c@Cell{..} = cellColor

createCellShiftFigure :: Field -> Cell -> Cell
createCellShiftFigure field c@Cell{..}  = case hc of 
        Nothing -> (Cell numLine numCell 2 colorBoard)
        Just hc1 ->  case (typeCell hc1) of
                        2 -> case (typeCell c) of
                                1 ->  (Cell numLine numCell 2 colorBoard)
                                0 -> c
                                2 -> c
                        0 -> c
                        1 -> (Cell numLine numCell 1 (getNewColor hc1))
        where hc = (higherCell c field)

shiftFigureOnField :: Field -> Field
--shiftFigureOnField _ = createField
shiftFigureOnField field = mapField (\c -> (createCellShiftFigure field c)) field

shiftFigure :: GameState -> GameState --сдвиг фигуры(на 1 вниз)
shiftFigure GameState{..}  = (GameState (shiftFigureOnField gameField) gameRandomGen gameFigures gameResult coordTetr colorTetr False) 

checkCanAddFigure :: Field -> CoordFigures -> Bool
checkCanAddFigure field [] = True
checkCanAddFigure field ((x, y) : xs) = (trace ("checkCan " ++ (show x) ++ (show y) ++ (show result) ++ (show (head field)) ++ (show (take 1 field)))) result
                        where result = ((typeCellFromField field x y ) == 2) && (checkCanAddFigure field xs)

newFigureOnField :: Field -> CoordFigures -> Color -> Field
newFigureOnField field [] color = field
newFigureOnField field ((x, y): xs) color = trace ((show "nfof ") ++ (show x) ++ " " ++ (show y) ++ " " ++ (show color))
         (newFigureOnField (changeCellInField field x y (Cell x y 1 color)) xs color)
--map (\(x, y) -> (changeCellInField field x y (Cell x y 1 color))) coords
--addFigureOnField field numb = map markFigure 

newFigureOnGame :: GameState  -> GameState -- падение новой фигуры 
newFigureOnGame GameState{..}
        | checkCanAddFigure gameField coordNextFigure = 
                (GameState (newFigureOnField gameField  coordNextFigure colorNextFigure) gameRandomGen (tail gameFigures) 
                        gameResult coordTetr colorTetr False )
        | otherwise = trace "checkEnd" (GameState gameField gameRandomGen gameFigures gameResult coordTetr colorTetr True)
            where nextFigure = (head gameFigures)
                  coordNextFigure = ((!!) coordTetr (nextFigure - 1))
                  colorNextFigure = ((!!) colorTetr (nextFigure - 1))
 


        
deleteLines :: Field -> Int -> (Field, Int) -- удаление линии со всеми заполненными  - пока не реализовано
deleteLines f r = (f, r)
              

checkFlyCell ::  Field -> Cell -> Bool
checkFlyCell field c = case lc of
        Nothing -> ((typeCell c) /= 1)
        Just lc1 -> ((typeCell c) /= 1) || ((typeCell c == 1) && (typeCell lc1 /= 0)) 
        where lc = lowerCell c field

haveFlyFigure :: Field -> Bool -- поменять!!!!!!!
haveFlyFigure field = {-trace ("haveFlyFigure" ++ show (take 3 field))-}
     (funFieldAll (checkFlyCell field) field) && (funFieldAny (\c -> ((typeCell c) == 1)) field)
--проверка -    1.что клетка - или не летящая, или летящая, но под ней свободно
--              2.что на поле есть летящие клетки
      
changeLandCellField :: Field -> Field
changeLandCellField field = mapField (\c@Cell{..} -> case (typeCell c) of
                                        1 -> (Cell numLine numCell 0 cellColor)
                                        otherwise -> c) field


changeLandCell :: GameState -> GameState
changeLandCell game@GameState{..} = 
        GameState (changeLandCellField gameField)  gameRandomGen  gameFigures gameResult coordTetr colorTetr False


handle :: Event -> GameState -> GameState
handle key game = game

createEnd ::GameState -> GameState
createEnd x = x --поменять!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! должно быть без параметров 
            

gameLoop :: Float -> GameState -> GameState
gameLoop _ game@GameState {..}  = 
        if (endGame) 
                then (createEnd game)
                else if (haveFlyFigure gameField)
                        then (shiftFigure game)
                        else  (newFigureOnGame.changeLandCell $ game)       


