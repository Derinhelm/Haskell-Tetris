module Type   where
import Graphics.Gloss

data Cell = Cell { numLine :: Int
                         , numCell :: Int --номер клетки в столбце
                         , cellType :: Int -- тип клетки 0 - занята(нижние клетки), 1 - занята(летящая клетка), 2 - свободна
                         , cellColor :: Color
                         } deriving Show
type Line = [Cell]
type Field = [Line] 

createLine :: Int -> Line
createLine x = [Cell x y 0 white | y <- [1..10]]

createField :: Field
createField = [createLine x| x <- [1.. 15]]