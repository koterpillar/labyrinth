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

showH :: Wall -> String
showH NoWall   = "  "
showH Wall     = "--"
showH HardWall = "=="

showV :: Wall -> String
showV NoWall   = " "
showV Wall     = "|"
showV HardWall = "‖"

data Player = Player { bullets  :: Int
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
                           }
                 deriving (Eq)

labWidth :: Labyrinth -> Int
labWidth = length . cells

labHeight :: Labyrinth -> Int
labHeight = length . head . cells

cell :: Labyrinth -> Int -> Int -> Cell
cell l x y = (cells l) !! x !! y

wallH :: Labyrinth -> Int -> Int -> Wall
wallH l x y = (wallsH l) !! x !! y

wallV :: Labyrinth -> Int -> Int -> Wall
wallV l x y = (wallsV l) !! x !! y

showWallLine :: Labyrinth -> Int -> String
showWallLine l y = mk ++ intercalate mk ws ++ mk
    where mk = "+"
          w  = labWidth l
          ws = map (\x -> showH $ wallH l x y) [0..w - 1]

showCellLine :: Labyrinth -> Int -> String
showCellLine l y = concat (map (\x -> showVWall l x y ++ showCell l x y) [0..w - 1])
                       ++ showVWall l w y
                   where w = labWidth l
                         showVWall :: Labyrinth -> Int -> Int -> String
                         showVWall l x y = showV $ wallV l x y
                         showCell :: Labyrinth -> Int -> Int -> String
                         showCell l x y = show $ cell l x y

instance Show Labyrinth where
    show l = intercalate "\n" $ firstLines ++ [lastLine]
        where h = labHeight l
              showLine l i = [showWallLine l i, showCellLine l i]
              firstLines = concat $ map (showLine l) [0..h - 1]
              lastLine = showWallLine l h
