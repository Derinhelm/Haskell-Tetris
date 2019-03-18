module Main where

import MyProj



main :: IO ()
main = do
    putStrLn "Enter your name: "
    name <- getLine
    result <- tetris
    writeFile "results" (name ++ " - " ++  (show result))