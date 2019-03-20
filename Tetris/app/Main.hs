module Main where

import MyProj
import Draw
import Type
import Graphics.Gloss.Interface.Pure.Game
import Constans
import System.Random


main :: IO ()
main = do
    putStrLn "Enter your name: "
    name <- getLine
    gen <- newStdGen
    let createGame = GameState (createField) gen (createManyFigures) 0 
    play window blue fps createGame drawGame handle gameLoop 
    --result из файла
    writeFile "results" (name ++ " - ")-- ++  (show result))
