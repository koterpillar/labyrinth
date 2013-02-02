-- Map-related functions
{-# Language TemplateHaskell #-}
module Labyrinth.Map where

import Control.Monad
import Control.Monad.State

import Peeker

data CellType = Land
                deriving (Eq)

data Cell = Cell { ctype_ :: CellType
                 }
                 deriving (Eq)

derivePeek ''Cell

data Wall = NoWall | Wall | HardWall
    deriving (Eq)

data Position = Pos { pX :: Int
                    , pY :: Int
                    }
                deriving (Eq)

instance Show Position where
    show (Pos x y) = "(" ++ (show x) ++ ", " ++ (show y) ++ ")"

data Direction = L | R | U | D
                 deriving (Eq, Show)

advance :: Position -> Direction -> Position
advance (Pos x y) L = Pos (x - 1) y
advance (Pos x y) U = Pos x (y - 1)
advance (Pos x y) R = Pos (x + 1) y
advance (Pos x y) D = Pos x (y + 1)

data Treasure = TrueTreasure | FakeTreasure
                deriving (Eq, Show)

data Player = Player { position_ :: Position
                     , bullets_  :: Int
                     , grenades_ :: Int
                     , treasure_ :: Maybe Treasure
                     }
              deriving (Eq)

derivePeek ''Player

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

emptyLabyrinth :: Int -> Int -> [Position] -> Labyrinth
emptyLabyrinth w h positions =
    let initialLab = Labyrinth { cells_         = replicate w $ replicate h $ (Cell Land)
                               , wallsH_        = replicate w $ replicate (h + 1) $ NoWall
                               , wallsV_        = replicate (w + 1) $ replicate h $ NoWall
                               , players_       = map initialPlayer positions
                               , currentPlayer_ = 0
                               }
    in (flip execState) initialLab $ do
        forM_ [0..w - 1] $ \x -> updS (wall (Pos x 0) U) HardWall
        forM_ [0..w - 1] $ \x -> updS (wall (Pos x (h - 1)) D) HardWall
        forM_ [0..h - 1] $ \y -> updS (wall (Pos 0 y) L) HardWall
        forM_ [0..h - 1] $ \y -> updS (wall (Pos (w - 1) y) R) HardWall

cell :: Position -> Peek Labyrinth Cell
cell (Pos x y) = cells ~> listP x ~> listP y

wallH :: Position -> Peek Labyrinth Wall
wallH (Pos x y) = wallsH ~> listP x ~> listP y

wallV :: Position -> Peek Labyrinth Wall
wallV (Pos x y) = wallsV ~> listP x ~> listP y

wall :: Position -> Direction -> Peek Labyrinth Wall
wall p U = wallH p
wall p L = wallV p
wall p D = wallH (advance p D)
wall p R = wallV (advance p R)

player :: Int -> Peek Labyrinth Player
player i = players ~> listP i
