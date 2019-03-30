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
    let createGame = GameState (createField) gen (createListFigures gen) 0 (createCoordFigures) (createColorFigures) False 28 0

    play window black fps createGame drawGame handleEvent update
    --result из файла
    --writeFile "results" (name ++ " - ")-- ++  (show result))
