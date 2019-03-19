module Main where

import MyProj
import Draw
import Type
import Graphics.Gloss.Interface.IO.Game



main :: IO ()
main = do
    putStrLn "Enter your name: "
    name <- getLine
    playIO window blue fps createField drawField handle gameLoop 
    --result из файла
    writeFile "results" (name ++ " - ")-- ++  (show result))