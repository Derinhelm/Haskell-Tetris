module Main where

import MyProj
import Draw
import Type
import Graphics.Gloss.Interface.IO.Game
import Constans
import System.Random


main :: IO ()
main = do
    putStrLn "Enter your name: "
    name <- getLine
    gen <- newStdGen
    let createGame = GameState name (createField) gen (createListFigures gen) 0 (createCoordFigures) (createColorFigures) Game 28 0

    playIO window black fps createGame drawGame handleEvent update
