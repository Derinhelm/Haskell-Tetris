module MyProj    where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Draw
import Type




shiftFigure :: Field -> Field
shiftFigure x = x

deleteLines :: Field -> Int -> (Field, Int)
deleteLines f r = (f, r)
checkEnd :: Field -> Bool
checkEnd _ = True
            
gameLoop :: Field -> Int ->IO Int --игровой цикл 
gameLoop oldField oldResult | checkEnd oldField = do 
                                drawField oldField
                                return oldResult
                            | otherwise = do
                                drawField oldField
                                drawField newField
                                gameLoop (shiftFigure newField) newResult
    where (newField, newResult) = deleteLines oldField oldResult


tetris :: IO Int  --функция, которая создает начальную картину игры, запускает первый игровой цикл, возвращает результат игры
tetris = gameLoop  createField 0