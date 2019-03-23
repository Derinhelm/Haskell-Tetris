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


createCellShiftFigure :: Field -> Cell -> Cell
createCellShiftFigure field c@Cell{..}  = case hc of 
        Nothing -> (Cell numLine numCell 2 colorBoard)
        Just hc1 ->  case (typeCell hc1) of
                        2 -> case (typeCell c) of
                                1 ->  (Cell numLine numCell 2 colorBoard)
                                0 -> c
                                2 -> c
                        0 -> c
                        1 -> (Cell numLine numCell 1 (getCellColor hc1))
        where hc = (higherCell c field)

shiftFigureOnField :: Field -> Field
--shiftFigureOnField _ = createField
shiftFigureOnField field = mapField (\c -> (createCellShiftFigure field c)) field

shiftFigure :: GameState -> GameState --сдвиг фигуры(на 1 вниз)
shiftFigure GameState{..}  = (GameState (shiftFigureOnField gameField) gameRandomGen gameFigures gameResult coordTetr colorTetr False) 


checkCanAddFigure :: Field -> CoordFigures -> Bool
checkCanAddFigure field [] = True
checkCanAddFigure field ((x, y) : xs) = ((typeCellFromField field x y ) == 2) && (checkCanAddFigure field xs)

newFigureOnField :: Field -> CoordFigures -> Color -> Field
newFigureOnField field [] color = field
newFigureOnField field ((x, y): xs) color = (newFigureOnField (changeCellInField field x y (Cell x y 1 color)) xs color)
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

checkCompletedLine :: Line -> Bool
checkCompletedLine line = funLineAll (\c -> ((typeCell c) == 0)) line

countDeletedLines :: Field -> Int -> Int
countDeletedLines [] res = res
countDeletedLines (x : xs) oldRes       | (checkCompletedLine x) = countDeletedLines xs (oldRes + 1)
                                        | otherwise = countDeletedLines xs oldRes
        
deleteLinesFromField :: Field -> Field
deleteLinesFromField f = f
--Сейчас считает(каждый проход отдельно) полные линии, не умеет их удалять
deleteLines :: GameState -> GameState -- (делаем перед добавлением новой фигуры)
--удаление линии со всеми заполненными  - пока не реализовано
deleteLines game@GameState{..} = 
        (GameState (deleteLinesFromField gameField) gameRandomGen gameFigures (gameResult + (countDeletedLines gameField 0))
                coordTetr colorTetr False)
              

checkFlyCell ::  Field -> Cell -> Bool
checkFlyCell field c = case lc of
        Nothing -> ((typeCell c) /= 1)
        Just lc1 -> ((typeCell c) /= 1) || ((typeCell c == 1) && (typeCell lc1 /= 0)) 
        where lc = lowerCell c field

haveFlyFigure :: Field -> Bool -- поменять!!!!!!!
haveFlyFigure field = (funFieldAll (checkFlyCell field) field) && (funFieldAny (\c -> ((typeCell c) == 1)) field)
--проверка -    1.что клетка - или не летящая, или летящая, но под ней свободно
--              2.что на поле есть летящие клетки
      
changeLandCellField :: Field -> Field
changeLandCellField field = mapField (\c@Cell{..} -> case (typeCell c) of
                                        1 -> (Cell numLine numCell 0 cellColor)
                                        otherwise -> c) field


changeLandCell :: GameState -> GameState
changeLandCell game@GameState{..} = 
        GameState (changeLandCellField gameField)  gameRandomGen  gameFigures gameResult coordTetr colorTetr False

        
       
checkCanMoveLeft:: Field -> Bool
checkCanMoveLeft field = funFieldAll (\c -> (((typeCell c) /= 1) ||
         ((isJust (leftCell c field)) && ((typeCell (fromJust (leftCell c field))) /= 0)))) field

createCellForMoveLeft :: Field -> Cell -> Cell
createCellForMoveLeft field c@Cell{..} = case (typeCell c) of 
        0 -> c
        otherwise ->  case (rightCell c field) of 
                Nothing -> (Cell numLine numCell 2 colorBoard)--просто клетка фона
                Just r -> case (typeCell r) of
                        2 -> (Cell numLine numCell 2 colorBoard) -- клетка фона 
                        1 -> (Cell numLine numCell 1 (getCellColor r))
                        0 -> (Cell numLine numCell 2 colorBoard) -- клетка фона 

moveLeft :: Field -> Field
moveLeft field  | checkCanMoveLeft field = mapField (createCellForMoveLeft field) field
                | otherwise = field                        

--проблема - мы уже поменяли правую, а теперь ее копируем???? в shift - этой проблемы нет, т.к. идем - сверху вниз                
        
checkCanMoveRight:: Field -> Bool
checkCanMoveRight field = funFieldAll (\c -> (((typeCell c) /= 1) ||
         ((isJust (rightCell c field)) && ((typeCell (fromJust (rightCell c field))) /= 0)))) field
                
createCellForMoveRight :: Field -> Cell -> Cell
createCellForMoveRight field c@Cell{..} = case (typeCell c) of 
        0 -> c
        otherwise ->  case (leftCell c field) of 
                Nothing -> (Cell numLine numCell 2 colorBoard)--просто клетка фона
                Just l -> case (typeCell l) of
                        2 -> (Cell numLine numCell 2 colorBoard) -- клетка фона 
                        1 -> (Cell numLine numCell 1 (getCellColor l))
                        0 -> (Cell numLine numCell 2 colorBoard) -- клетка фона 

moveRight :: Field -> Field
moveRight field  | checkCanMoveRight field = mapField (createCellForMoveRight field) field
                 | otherwise = field                         



-- Handle events.
handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) game@GameState{..} = 
        GameState (moveLeft gameField)  gameRandomGen  gameFigures gameResult coordTetr colorTetr False
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) game@GameState{..} = 
        GameState (moveRight gameField)  gameRandomGen  gameFigures gameResult coordTetr colorTetr False
handleEvent _ g = g





createEnd ::GameState -> GameState
createEnd x = x --поменять!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! должно быть без параметров 
            

gameLoop :: Float -> GameState -> GameState
gameLoop _ game@GameState {..}  = 
        if (endGame) 
                then (createEnd game)
                else if (haveFlyFigure gameField)
                        then (shiftFigure game)
                        else  (newFigureOnGame.deleteLines.changeLandCell $ game)       


