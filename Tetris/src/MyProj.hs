{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}


module MyProj    where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Draw
import Type
import Constans
import Data.Maybe
import Debug.Trace
import Data.List

createCellShiftFigure :: Field -> Cell -> Cell
createCellShiftFigure field c@Cell{..}  = case hc of 
        Nothing -> (Cell numLine numCell 2 colorBoard)
        Just hc1 ->  case (getCellType hc1) of
                        2 -> case (getCellType c) of
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
shiftFigure GameState{..}  = 
        (GameState (shiftFigureOnField gameField) gameRandomGen gameFigures gameResult coordTetr colorTetr False rotateTypeFigure) 


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
                        gameResult coordTetr colorTetr False rotType)
        | otherwise =
                 trace "checkEnd" (GameState gameField gameRandomGen gameFigures gameResult coordTetr colorTetr True rotateTypeFigure)
            where nextFigure = (head gameFigures)
                  rotType = if (nextFigure == 0) then 0  else nextFigure * 4 - 3
                  coordNextFigure = ((!!) coordTetr (nextFigure))
                  colorNextFigure = ((!!) colorTetr (nextFigure))

checkCompletedLine :: Line -> Bool
checkCompletedLine line = funLineAll (\c -> ((getCellType c) == 0)) line

countDeletedLines :: Field -> Int -> Int
countDeletedLines [] res = res
countDeletedLines (x : xs) oldRes       | (checkCompletedLine x) = countDeletedLines xs (oldRes + 1)
                                        | otherwise = countDeletedLines xs oldRes
        
numCompLine :: Field -> Int -> Int
numCompLine [] _ = -1
numCompLine (x:xs) numberLine   | (checkCompletedLine x) = numberLine
                                | otherwise = (numCompLine xs (numberLine + 1))

deleteLineWithNumber :: Field -> Int -> Field
deleteLineWithNumber field numDeletedLine = mapField (\c@Cell{..} -> 
        if (numLine > numDeletedLine) 
                then c
                else case (higherCell c field) of
                        Nothing -> (Cell numLine numCell 2 colorBoard) --верхний ряд
                        Just hc ->  (Cell numLine numCell (getCellType hc) (getCellColor hc))
        ) field


deleteLinesFromField :: Field -> Field
deleteLinesFromField field | (curCompLine == -1) = field 
                           | otherwise = (deleteLinesFromField (deleteLineWithNumber field curCompLine))
                where curCompLine = (numCompLine field 0)

--Сейчас считает(каждый проход отдельно) полные линии, не умеет их удалять
deleteLines :: GameState -> GameState -- (делаем перед добавлением новой фигуры)
--удаление линии со всеми заполненными  - пока не реализовано
deleteLines game@GameState{..} = 
        (GameState (deleteLinesFromField gameField) gameRandomGen gameFigures (gameResult + (countDeletedLines gameField 0))
                coordTetr colorTetr False rotateTypeFigure)
              

checkFlyCell ::  Field -> Cell -> Bool
checkFlyCell field c = case lc of
        Nothing -> ((getCellType c) /= 1)
        Just lc1 -> ((getCellType c) /= 1) || ((getCellType c == 1) && (getCellType lc1 /= 0)) 
        where lc = lowerCell c field

haveFlyFigure :: Field -> Bool -- поменять!!!!!!!
haveFlyFigure field = (funFieldAll (checkFlyCell field) field) && (funFieldAny (\c -> ((getCellType c) == 1)) field)
--проверка -    1.что клетка - или не летящая, или летящая, но под ней свободно
--              2.что на поле есть летящие клетки
      
changeLandCellField :: Field -> Field
changeLandCellField field = mapField (\c@Cell{..} -> case (getCellType c) of
                                        1 -> (Cell numLine numCell 0 cellColor)
                                        otherwise -> c) field


changeLandCell :: GameState -> GameState
changeLandCell game@GameState{..} = 
        GameState (changeLandCellField gameField) gameRandomGen gameFigures gameResult coordTetr colorTetr False rotateTypeFigure

        
       
checkCanMoveLeft:: Field -> Bool
checkCanMoveLeft field = funFieldAll (\c -> (((getCellType c) /= 1) ||
         ((isJust (leftCell c field)) && ((getCellType (fromJust (leftCell c field))) /= 0)))) field

createCellForMoveLeft :: Field -> Cell -> Cell
createCellForMoveLeft field c@Cell{..} = case (getCellType c) of 
        0 -> c
        otherwise ->  case (rightCell c field) of 
                Nothing -> (Cell numLine numCell 2 colorBoard)--просто клетка фона
                Just r -> case (getCellType r) of
                        2 -> (Cell numLine numCell 2 colorBoard) -- клетка фона 
                        1 -> (Cell numLine numCell 1 (getCellColor r))
                        0 -> (Cell numLine numCell 2 colorBoard) -- клетка фона 

moveLeft :: Field -> Field
moveLeft field  | checkCanMoveLeft field = mapField (createCellForMoveLeft field) field
                | otherwise = field                        

        
checkCanMoveRight:: Field -> Bool
checkCanMoveRight field = funFieldAll (\c -> (((getCellType c) /= 1) ||
         ((isJust (rightCell c field)) && ((getCellType (fromJust (rightCell c field))) /= 0)))) field
                
createCellForMoveRight :: Field -> Cell -> Cell
createCellForMoveRight field c@Cell{..} = case (getCellType c) of 
        0 -> c
        otherwise ->  case (leftCell c field) of 
                Nothing -> (Cell numLine numCell 2 colorBoard)--просто клетка фона
                Just l -> case (getCellType l) of
                        2 -> (Cell numLine numCell 2 colorBoard) -- клетка фона 
                        1 -> (Cell numLine numCell 1 (getCellColor l))
                        0 -> (Cell numLine numCell 2 colorBoard) -- клетка фона 

moveRight :: Field -> Field
moveRight field  | checkCanMoveRight field = mapField (createCellForMoveRight field) field
                 | otherwise = field              
                 
getCoordsFromCells :: [Cell] -> [(Int, Int)] -> [(Int, Int)]
getCoordsFromCells [] coords = coords
getCoordsFromCells (c@Cell{..}:cs) coords = getCoordsFromCells cs (coords ++ [(numLine, numCell)])
                


findCoordFigure :: Field -> CoordFigures
findCoordFigure field = (getCoordsFromCells goodCells [])
        where goodCells = (findCellCond field (\ c -> ((getCellType c) == 1)) [])


compareFigure :: CoordFigures -> Int -> [(CoordFigures, CoordCell)] -> (Int, CoordCell)
compareFigure [(x1, y1), (x2, y2), (x3, y3), (x4, y4)] numModel 
        (([(x5, y5), (x6, y6), (x7, y7), (x8, y8)], mainCell):zs) 
                | (x1 == x5) && (y1 == y5) && (x2 == x6) && (y2 == y6) && (x3 == x7) && (y3 == y7) && (x4 == x8) && (y4 == y8) = 
                        (trace ((show [(x1, y1), (x2, y2), (x3, y3), (x4, y4)]) ++ " ok " ++ (show numModel)))(numModel, mainCell)
                | otherwise =  (trace ((show [(x1, y1), (x2, y2), (x3, y3), (x4, y4)]) ++ "no")) compareFigure [(x1, y1), (x2, y2), (x3, y3), (x4, y4)] (numModel + 1) zs
                
funForSortBy :: (Int, Int) -> (Int, Int) -> Ordering
funForSortBy (x1, y1) (x2, y2)  | (x1 < y1 || ((x1 == y1) && (x2 < y2))) = LT
                                | otherwise = GT



numberNextRotateModel :: Int -> Int
numberNextRotateModel oldNum = case oldNum of
        0 -> 0
        4 -> 1
        8 -> 5
        12 -> 9
        16 -> 13
        20 -> 17
        24 -> 21
        28 -> 25
        otherwise -> oldNum + 1

createNewCoord :: Int -> CoordCell -> [CoordCell]
createNewCoord numOldModel (x, y) = (trace ("cnc " ++(show numOldModel) ++ (show ((x, y))))) itogCoord
        where   numNewModel = (numberNextRotateModel numOldModel)
                newModel = ((!!) (createRotateModels) numNewModel)
                newMainCoord = (snd newModel)
                itogCoord = map (\coordNewModel -> 
                        (x - (fst newMainCoord) + (fst coordNewModel), y - (snd newMainCoord) + (snd coordNewModel))) (fst newModel)


checkCanRotate :: Field -> CoordFigures -> Bool
checkCanRotate field [] = True
checkCanRotate field ((x, y) : xs) = (x >= 0) && (y >= 0) && (x < 15) && (y < 10) && ((typeCellFromField field x y) == 2) && (checkCanRotate field xs)

setNewCellsdForRotate :: [CoordCell] -> Color -> Field -> Field --ставим новые клетки 
setNewCellsdForRotate [] color field = field
setNewCellsdForRotate ((x, y): xs) color field = setNewCellsdForRotate xs color (changeCellInField field x y (Cell x y 1 color))

deleteOldCellsForRotate :: [CoordCell] -> Color -> Field -> Field -- удаляем старые клетки
deleteOldCellsForRotate [] color field = field
deleteOldCellsForRotate ((x, y): xs) color field = deleteOldCellsForRotate xs color (changeCellInField field x y (Cell x y 2 colorBoard))


rotateFigure :: Field -> Int-> Field
rotateFigure field  rt    | (checkCanRotate fieldDeletedOldCells newCoord) = setNewCellsdForRotate newCoord color fieldDeletedOldCells
                        | otherwise = field
                        where   oldCoord = (findCoordFigure field)
                                offsetX = minimum (map fst oldCoord)
                                offsetY = minimum (map snd oldCoord)
                                color = cellColor (getCell field (fst (head oldCoord)) (snd (head oldCoord)))
                                (oldRelMainX,  oldRelMainY) = (snd ((!!) (createRotateModels) rt)) 
                                newCoord = createNewCoord rt (offsetX + oldRelMainX, offsetY + oldRelMainY)
                                fieldDeletedOldCells = (trace ("newcoord" ++ (show newCoord))) deleteOldCellsForRotate oldCoord color field







-- Handle events.
handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) game@GameState{..} = 
    GameState (moveLeft gameField)  gameRandomGen gameFigures gameResult coordTetr colorTetr False rotateTypeFigure
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) game@GameState{..} = 
    GameState (moveRight gameField) gameRandomGen gameFigures gameResult coordTetr colorTetr False rotateTypeFigure
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) game@GameState{..} = 
        GameState (createField) gameRandomGen (createListFigures gameRandomGen) 0 (createCoordFigures) (createColorFigures) False 28  
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) game@GameState{..} = (trace (show rotateTypeFigure))
    GameState (rotateFigure gameField rotateTypeFigure) gameRandomGen
        gameFigures gameResult coordTetr colorTetr False q
        where q = (numberNextRotateModel rotateTypeFigure)
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




--random генерация, отображение следующей, ускорение 