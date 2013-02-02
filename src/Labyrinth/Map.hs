-- Map-related functions
{-# Language TemplateHaskell #-}
module Labyrinth.Map where

import Data.List

import Peeker

data CellType = Land
                deriving (Eq)

instance Show CellType where
    show Land = "."

data Cell = Cell { ctype_ :: CellType
                 }
                 deriving (Eq)

derivePeek ''Cell

instance Show Cell where
    show (Cell t) = show t ++ " "

data Wall = NoWall | Wall | HardWall
    deriving (Eq)

data Position = Pos { pX :: Int
                    , pY :: Int
                    }
                deriving (Eq)

instance Show Position where
    show (Pos x y) = "(" ++ (show x) ++ ", " ++ (show y) ++ ")"

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

data Treasure = TrueTreasure | FakeTreasure
                deriving (Eq, Show)

data Player = Player { position_ :: Position
                     , bullets_  :: Int
                     , grenades_ :: Int
                     , treasure_ :: Maybe Treasure
                     }
              deriving (Eq)

derivePeek ''Player

instance Show Player where
    show p = "Player " ++ show (position_ p)

initialPlayer :: Position -> Player
initialPlayer pos = Player { position_ = pos
                           , bullets_  = 3
                           , grenades_ = 3
                           , treasure_ = Nothing
                           }

-- wallsV and wallsH are considered to be to the left and top of the cells
data Labyrinth = Labyrinth { cells_         :: [[Cell]]
                           , wallsH_        :: [[Wall]]
                           , wallsV_        :: [[Wall]]
                           , players_       :: [Player]
                           , currentPlayer_ :: Int
                           }
                 deriving (Eq)

derivePeek ''Labyrinth

labWidth :: Labyrinth -> Int
labWidth = length . cells_

labHeight :: Labyrinth -> Int
labHeight = length . head . cells_

playerCount :: Labyrinth -> Int
playerCount = length . players_

cell :: Position -> Peek Labyrinth Cell
cell (Pos x y) = cells ~> listP x ~> listP y

wallH :: Position -> Peek Labyrinth Wall
wallH (Pos x y) = wallsH ~> listP x ~> listP y

wallV :: Position -> Peek Labyrinth Wall
wallV (Pos x y) = wallsV ~> listP x ~> listP y

wall :: Position -> Direction -> Peek Labyrinth Wall
wall _ Next = error "No walls in Next direction."
wall p U = wallV p
wall p L = wallH p
wall p D = wallV (advance p D)
wall p R = wallH (advance p R)

player :: Int -> Peek Labyrinth Player
player i = players ~> listP i

showWallLine :: Labyrinth -> Int -> String
showWallLine l y = mk ++ intercalate mk ws ++ mk
    where mk = "+"
          w  = labWidth l
          ws = map (\x -> showH $ getP (wallH (Pos x y)) l) [0..w - 1]

showCellLine :: Labyrinth -> Int -> String
showCellLine l y = concat (map (\x -> showVWall l (Pos x y) ++ showCell l (Pos x y)) [0..w - 1])
                       ++ showVWall l (Pos w y)
                   where w = labWidth l
                         showVWall :: Labyrinth -> Position -> String
                         showVWall l p = showV $ getP (wallV p) l
                         showCell :: Labyrinth -> Position -> String
                         showCell l p = show $ getP (cell p) l

showMap :: Labyrinth -> [String]
showMap l = firstLines ++ [lastLine]
    where h = labHeight l
          showLine l i = [showWallLine l i, showCellLine l i]
          firstLines = concat $ map (showLine l) [0..h - 1]
          lastLine = showWallLine l h

showPlayers :: Labyrinth -> [String]
showPlayers l = map (uncurry showPlayer) $ zip (getP players l) [0..]
    where showPlayer p i = (show i) ++ ": " ++ (show p)

showCurrentPlayer :: Labyrinth -> [String]
showCurrentPlayer l = ["Current player: " ++ show (getP currentPlayer l)]

instance Show Labyrinth where
    show l = intercalate "\n" $ foldr1 (++) parts
        where parts = map ($ l) [ showMap
                                , const [""]
                                , showPlayers
                                , showCurrentPlayer
                                ]
