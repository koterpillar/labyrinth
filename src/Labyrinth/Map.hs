-- Map-related functions
module Labyrinth.Map where

import Data.List

data CellType = Land
                deriving (Eq)

instance Show CellType where
    show Land = "."

data Cell = Cell CellType
            deriving (Eq)

instance Show Cell where
    show (Cell t) = show t ++ " "

data Wall = NoWall | Wall | HardWall
    deriving (Eq)

data Position = Pos { pX :: Int
                    , pY :: Int
                    }
                deriving (Eq, Show)

data Direction = L | R | U | D | Next
                 deriving (Eq, Show)

advance :: Position -> Direction -> Position
advance (Pos x y) L = Pos (x - 1) y
advance (Pos x y) U = Pos x (y - 1)
advance (Pos x y) R = Pos (x + 1) y
advance (Pos x y) D = Pos x (y + 1)
advance (Pos x y) Next = error "Cannot advance in Next direction."

showH :: Wall -> String
showH NoWall   = "  "
showH Wall     = "--"
showH HardWall = "=="

showV :: Wall -> String
showV NoWall   = " "
showV Wall     = "|"
showV HardWall = "‖"

data Player = Player { position :: Position
                     , bullets  :: Int
                     , grenades :: Int
                     , treasure :: Maybe Treasure
                     }
              deriving (Eq)

data Treasure = TrueTreasure | FakeTreasure
                deriving (Eq)

-- wallsV and wallsH are considered to be to the left and top of the cells
data Labyrinth = Labyrinth { cells   :: [[Cell]]
                           , wallsH  :: [[Wall]]
                           , wallsV  :: [[Wall]]
                           , players :: [Player]
                           , currentPlayer :: Int
                           }
                 deriving (Eq)

labWidth :: Labyrinth -> Int
labWidth = length . cells

labHeight :: Labyrinth -> Int
labHeight = length . head . cells

cell :: Labyrinth -> Position -> Cell
cell l (Pos x y) = (cells l) !! x !! y

wallH :: Labyrinth -> Position -> Wall
wallH l (Pos x y) = (wallsH l) !! x !! y

wallV :: Labyrinth -> Position -> Wall
wallV l (Pos x y) = (wallsV l) !! x !! y

wallAt :: Labyrinth -> Position -> Direction -> Wall
wallAt _ _ Next = error "No walls in Next direction."
wallAt l p U = wallV l p
wallAt l p L = wallH l p
wallAt l p D = wallV l (advance p D)
wallAt l p R = wallH l (advance p R)

player :: Labyrinth -> Int -> Player
player l i = (players l) !! i

showWallLine :: Labyrinth -> Int -> String
showWallLine l y = mk ++ intercalate mk ws ++ mk
    where mk = "+"
          w  = labWidth l
          ws = map (\x -> showH $ wallH l (Pos x y)) [0..w - 1]

showCellLine :: Labyrinth -> Int -> String
showCellLine l y = concat (map (\x -> showVWall l (Pos x y) ++ showCell l (Pos x y)) [0..w - 1])
                       ++ showVWall l (Pos w y)
                   where w = labWidth l
                         showVWall :: Labyrinth -> Position -> String
                         showVWall l p = showV $ wallV l p
                         showCell :: Labyrinth -> Position -> String
                         showCell l p = show $ cell l p

instance Show Labyrinth where
    show l = intercalate "\n" $ firstLines ++ [lastLine]
        where h = labHeight l
              showLine l i = [showWallLine l i, showCellLine l i]
              firstLines = concat $ map (showLine l) [0..h - 1]
              lastLine = showWallLine l h
