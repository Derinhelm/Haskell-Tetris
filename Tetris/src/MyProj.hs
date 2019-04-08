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
        Nothing -> (Cell numLine numCell Emp colorBoard)
        Just hc1 ->  case (getCellType hc1) of
                        Emp -> case (getCellType c) of
                                Fly ->  (Cell numLine numCell Emp colorBoard)
                                Land -> c
                                Emp -> c
                        Land -> c
                        Fly -> (Cell numLine numCell Fly (getCellColor hc1))
        where hc = (higherCell c field)

shiftFigureOnField :: Field -> Field
shiftFigureOnField field = mapField (\c -> (createCellShiftFigure field c)) field

shiftFigure :: GameState -> GameState --сдвиг фигуры(на 1 вниз)
shiftFigure GameState{..}  = 
        (GameState userName (shiftFigureOnField gameField) gameRandomGen gameFigures gameResult coordTetr colorTetr Game rotateTypeFigure 
                (numLoop + 1)) 


checkCanAddFigure :: Field -> CoordFigures -> Bool
checkCanAddFigure field [] = True
checkCanAddFigure field ((x, y) : xs) = ((typeCellFromField field x y ) == Emp) && (checkCanAddFigure field xs)

newFigureOnField :: Field -> CoordFigures -> Color -> Field
newFigureOnField field [] color = field
newFigureOnField field ((x, y): xs) color = (newFigureOnField (changeCellInField field x y (Cell x y Fly color)) xs color)


newFigureOnGame :: GameState  -> GameState -- падение новой фигуры 
newFigureOnGame GameState{..}
        | checkCanAddFigure gameField coordNextFigure = 
                (GameState userName (newFigureOnField gameField  coordNextFigure colorNextFigure) gameRandomGen (tail gameFigures) 
                        gameResult coordTetr colorTetr Game rotType (numLoop + 1))
        | otherwise =
                 (GameState userName gameField gameRandomGen gameFigures gameResult coordTetr colorTetr FirstEnd rotateTypeFigure
                        (numLoop + 1))
            where       nextFigure = (head gameFigures)
                        rotType = if (nextFigure == 0) then 0  else nextFigure * 4 - 3
                        coordNextFigure = ((!!) coordTetr (nextFigure))
                        colorNextFigure = ((!!) colorTetr (nextFigure))

checkCompletedLine :: Line -> Bool
checkCompletedLine line = funLineAll (\c -> ((getCellType c) == Land)) line

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
                        Nothing -> (Cell numLine numCell Emp colorBoard) --верхний ряд
                        Just hc ->  (Cell numLine numCell (getCellType hc) (getCellColor hc))
        ) field


deleteLinesFromField :: Field -> Field
deleteLinesFromField field | (curCompLine == -1) = field 
                           | otherwise = (deleteLinesFromField (deleteLineWithNumber field curCompLine))
                where curCompLine = (numCompLine field 0)


deleteLines :: GameState -> GameState -- (делаем перед добавлением новой фигуры)
--удаление линии со всеми заполненными 
deleteLines game@GameState{..} = 
        (GameState userName (deleteLinesFromField gameField) gameRandomGen gameFigures (gameResult + (countDeletedLines gameField 0))
                coordTetr colorTetr Game rotateTypeFigure numLoop)
              

checkFlyCell ::  Field -> Cell -> Bool
checkFlyCell field c = case lc of
        Nothing -> ((getCellType c) /= Fly)
        Just lc1 -> ((getCellType c) /= Fly) || ((getCellType c == Fly) && (getCellType lc1 /= Land)) 
        where lc = lowerCell c field

haveFlyFigure :: Field -> Bool
haveFlyFigure field = (funFieldAll (checkFlyCell field) field) && (funFieldAny (\c -> ((getCellType c) == Fly)) field)
--проверка -    1.что клетка - или не летящая, или летящая, но под ней свободно
--              2.что на поле есть летящие клетки
      
changeLandCellField :: Field -> Field
changeLandCellField field = mapField (\c@Cell{..} -> case (getCellType c) of
                                        Fly -> (Cell numLine numCell Land cellColor)
                                        otherwise -> c) field


changeLandCell :: GameState -> GameState
changeLandCell game@GameState{..} = 
        GameState userName (changeLandCellField gameField) gameRandomGen gameFigures gameResult coordTetr colorTetr endGame
                rotateTypeFigure numLoop

        
       
checkCanMoveLeft:: Field -> Bool
checkCanMoveLeft field = funFieldAll (\c -> (((getCellType c) /= Fly) ||
         ((isJust (leftCell c field)) && ((getCellType (fromJust (leftCell c field))) /= Land)))) field

createCellForMoveLeft :: Field -> Cell -> Cell
createCellForMoveLeft field c@Cell{..} = case (getCellType c) of 
        Land -> c
        otherwise ->  case (rightCell c field) of 
                Nothing -> (Cell numLine numCell Emp colorBoard)--просто клетка фона
                Just r -> case (getCellType r) of
                        Emp -> (Cell numLine numCell Emp colorBoard) -- клетка фона 
                        Fly -> (Cell numLine numCell Fly (getCellColor r))
                        Land -> (Cell numLine numCell Emp colorBoard) -- клетка фона 

moveLeft :: Field -> Field
moveLeft field  | checkCanMoveLeft field = mapField (createCellForMoveLeft field) field
                | otherwise = field                        

        
checkCanMoveRight:: Field -> Bool
checkCanMoveRight field = funFieldAll (\c -> (((getCellType c) /= Fly) ||
         ((isJust (rightCell c field)) && ((getCellType (fromJust (rightCell c field))) /= Land)))) field
                
createCellForMoveRight :: Field -> Cell -> Cell
createCellForMoveRight field c@Cell{..} = case (getCellType c) of 
        Land -> c
        otherwise ->  case (leftCell c field) of 
                Nothing -> (Cell numLine numCell Emp colorBoard)--просто клетка фона
                Just l -> case (getCellType l) of
                        Emp -> (Cell numLine numCell Emp colorBoard) -- клетка фона 
                        Fly -> (Cell numLine numCell Fly (getCellColor l))
                        Land -> (Cell numLine numCell Emp colorBoard) -- клетка фона 

moveRight :: Field -> Field
moveRight field  | checkCanMoveRight field = mapField (createCellForMoveRight field) field
                 | otherwise = field              
                 
getCoordsFromCells :: [Cell] -> [(Int, Int)] -> [(Int, Int)]
getCoordsFromCells [] coords = coords
getCoordsFromCells (c@Cell{..}:cs) coords = getCoordsFromCells cs (coords ++ [(numLine, numCell)])
                


findCoordFigure :: Field -> CoordFigures
findCoordFigure field = (getCoordsFromCells goodCells [])
        where goodCells = (findCellCond field (\ c -> ((getCellType c) == Fly)) [])


compareFigure :: CoordFigures -> Int -> [(CoordFigures, CoordCell)] -> (Int, CoordCell)
compareFigure [(x1, y1), (x2, y2), (x3, y3), (x4, y4)] numModel 
        (([(x5, y5), (x6, y6), (x7, y7), (x8, y8)], mainCell):zs) 
                | (x1 == x5) && (y1 == y5) && (x2 == x6) && (y2 == y6) && (x3 == x7) && (y3 == y7) && (x4 == x8) && (y4 == y8) = 
                        (numModel, mainCell)
                | otherwise = compareFigure [(x1, y1), (x2, y2), (x3, y3), (x4, y4)] (numModel + 1) zs
                
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
createNewCoord numOldModel (x, y) = itogCoord
        where   numNewModel = (numberNextRotateModel numOldModel)
                newModel = ((!!) (createRotateModels) numNewModel)
                newMainCoord = (snd newModel)
                itogCoord = map (\coordNewModel -> 
                        (x - (fst newMainCoord) + (fst coordNewModel), y - (snd newMainCoord) + (snd coordNewModel))) (fst newModel)


checkCanRotate :: Field -> CoordFigures -> Bool
checkCanRotate field [] = True
checkCanRotate field ((x, y) : xs) = (x >= 0) && (y >= 0) && (x < 15) && (y < 10) && ((typeCellFromField field x y) == Emp) && (checkCanRotate field xs)

setNewCellsdForRotate :: [CoordCell] -> Color -> Field -> Field --ставим новые клетки 
setNewCellsdForRotate [] color field = field
setNewCellsdForRotate ((x, y): xs) color field = setNewCellsdForRotate xs color (changeCellInField field x y (Cell x y Fly color))

deleteOldCellsForRotate :: [CoordCell] -> Color -> Field -> Field -- удаляем старые клетки
deleteOldCellsForRotate [] color field = field
deleteOldCellsForRotate ((x, y): xs) color field = deleteOldCellsForRotate xs color (changeCellInField field x y (Cell x y Emp colorBoard))


rotateFigure :: Field -> Int-> Field
rotateFigure field  rt  | (checkCanRotate fieldDeletedOldCells newCoord) = setNewCellsdForRotate newCoord color fieldDeletedOldCells
                        | otherwise = field
                        where   oldCoord = (findCoordFigure field)
                                offsetX = minimum (map fst oldCoord)
                                offsetY = minimum (map snd oldCoord)
                                color = cellColor (getCell field (fst (head oldCoord)) (snd (head oldCoord)))
                                (oldRelMainX,  oldRelMainY) = (snd ((!!) (createRotateModels) rt)) 
                                newCoord = createNewCoord rt (offsetX + oldRelMainX, offsetY + oldRelMainY)
                                fieldDeletedOldCells = deleteOldCellsForRotate oldCoord color field







-- Handle events.
handleEvent :: Event -> GameState -> IO GameState
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) game@GameState{..} =
        if (endGame == Game) 
                then return (GameState userName (moveLeft gameField)  gameRandomGen 
                        gameFigures gameResult coordTetr colorTetr endGame rotateTypeFigure numLoop)
                else return game
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) game@GameState{..} = 
        if (endGame == Game) 
                then    return (GameState userName (moveRight gameField) gameRandomGen gameFigures 
                        gameResult coordTetr colorTetr endGame rotateTypeFigure numLoop)
                else    return game
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) game@GameState{..} = 
        return (GameState userName (createField) gameRandomGen (tail (tail gameFigures))  0 coordTetr
                colorTetr Game 28 0)
        
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) game@GameState{..} =
        if (endGame == Game) 
                then    return (GameState userName (rotateFigure gameField rotateTypeFigure) gameRandomGen 
                        gameFigures gameResult coordTetr colorTetr endGame q numLoop)
                else    return game
        where   q = (numberNextRotateModel rotateTypeFigure)

handleEvent _ g = return g




createEnd ::GameState -> IO GameState
createEnd game@GameState{..} = do
        if (endGame == FirstEnd)
                then do
                        (trace ("append" ++ (show numLoop))) appendFile "result" (((show userName) ++ " - ") ++  (show gameResult) ++ "\n")
                        return (GameState userName gameField gameRandomGen 
                                gameFigures gameResult coordTetr colorTetr OldEnd rotateTypeFigure numLoop)
                else    return game
            

gameLoop :: IO GameState -> IO GameState
gameLoop g = 
        do        
        game@GameState {..} <- g
        if (endGame /= Game) 
                then (createEnd game)
                else if (haveFlyFigure gameField)
                        then (return (shiftFigure game))
                        else  (return (newFigureOnGame.deleteLines.changeLandCell $ game))

runGameLoop :: Int -> IO GameState -> IO GameState
runGameLoop num g = 
        do
                game@GameState{..} <- g
                if (num == 0)
                        then g
                        else runGameLoop (num - 1) (gameLoop (return game)) 
    



update :: Float -> GameState -> IO GameState
update _ game@GameState{..} = runGameLoop (min x 3) (return game)
                        where x = (ceiling  (((realToFrac numLoop) / 30) + 0.0001))

--проблема - если результат 0, и было хоть одно нажатие влево/вправо, то два раза пишется