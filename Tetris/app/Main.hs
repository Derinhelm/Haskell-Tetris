module Main where

import MyProj
import Draw
import Type
import Graphics.Gloss.Interface.Pure.Game
import Constans
import System.Random


main :: IO ()
main = do
    --putStrLn "Enter your name: "
    --name <- getLine
    gen <- newStdGen
    let createGame = GameState (createField) gen (createListFigures) 0 (createCoordFigures) (createColorFigures) False
    --a <- return (gameLoop 0 (createGame))
    --b <- return (gameLoop 0 a)
    --c <- return (gameLoop 0 b)
    --putStrLn(show a)
    --putStrLn(show b)
    --putStrLn(show c)
    play window black fps createGame drawGame handleEvent gameLoop 
    --result из файла
    --writeFile "results" (name ++ " - ")-- ++  (show result))
