{-# Language TemplateHaskell #-}
module Labyrinth.Map where

import Control.Monad
import Control.Monad.State

import Data.List
import Data.Maybe

import Peeker

data Direction = L | R | U | D
                 deriving (Eq)

data CellType = Land
              | Armory
              | Hospital
              | Pit { pitNumber_ :: Int }
              | River { riverDirection_ :: Direction }
              | RiverDelta
              deriving (Eq)

derivePeek ''CellType

data Treasure = TrueTreasure | FakeTreasure
                deriving (Eq)

data Cell = Cell { ctype_      :: CellType
                 , cbullets_   :: Int
                 , cgrenades_  :: Int
                 , ctreasures_ :: [Treasure]
                 }
                 deriving (Eq)

derivePeek ''Cell

emptyCell :: CellType -> Cell
emptyCell ct = Cell { ctype_      = ct
                    , cbullets_   = 0
                    , cgrenades_  = 0
                    , ctreasures_ = []
                    }

data Wall = NoWall | Wall | HardWall
    deriving (Eq)

data Position = Pos { pX :: Int
                    , pY :: Int
                    }
                deriving (Eq)

instance Show Position where
    show (Pos x y) = "(" ++ (show x) ++ ", " ++ (show y) ++ ")"

advance :: Position -> Direction -> Position
advance (Pos x y) L = Pos (x - 1) y
advance (Pos x y) U = Pos x (y - 1)
advance (Pos x y) R = Pos (x + 1) y
advance (Pos x y) D = Pos x (y + 1)

data Health = Healthy | Wounded | Dead
              deriving (Eq)

data Player = Player { position_  :: Position
                     , phealth_   :: Health
                     , pbullets_  :: Int
                     , pgrenades_ :: Int
                     , ptreasure_ :: Maybe Treasure
                     }
              deriving (Eq)

derivePeek ''Player

maxBullets :: Int
maxBullets = 3

maxGrenades :: Int
maxGrenades = 3

initialPlayer :: Position -> Player
initialPlayer pos = Player { position_  = pos
                           , phealth_   = Healthy
                           , pbullets_  = maxBullets
                           , pgrenades_ = maxGrenades
                           , ptreasure_ = Nothing
                           }

type PlayerId = Int

-- wallsV and wallsH are considered to be to the left and top of the cells
data Labyrinth = Labyrinth { cells_         :: [[Cell]]
                           , wallsH_        :: [[Wall]]
                           , wallsV_        :: [[Wall]]
                           , players_       :: [Player]
                           , currentPlayer_ :: PlayerId
                           }
                 deriving (Eq)

derivePeek ''Labyrinth

labWidth :: Labyrinth -> Int
labWidth = length . cells_

labHeight :: Labyrinth -> Int
labHeight = length . head . cells_

isInside :: Position -> Labyrinth -> Bool
isInside (Pos x y) l = and [ x >= 0
                            , x < w
                            , y >= 0
                            , y <= h
                            ]
    where w = labWidth l
          h = labHeight l

isOutside :: Position -> Labyrinth -> Bool
isOutside p = not . isInside p

outerPos :: Labyrinth -> [(Position, Direction)]
outerPos l = concat [ [(Pos x 0, U)       | x <- [0..w - 1]]
                    , [(Pos x (h - 1), D) | x <- [0..w - 1]]
                    , [(Pos 0 y, L)       | y <- [0..h - 1]]
                    , [(Pos (w - 1) y, R) | y <- [0..h - 1]]
                    ]
    where w = labWidth l
          h = labHeight l

playerCount :: Labyrinth -> Int
playerCount = length . players_

emptyLabyrinth :: Int -> Int -> [Position] -> Labyrinth
emptyLabyrinth w h positions =
    let initialLab = Labyrinth { cells_         = replicate w $ replicate h $ emptyCell Land
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

player :: PlayerId -> Peek Labyrinth Player
player i = players ~> listP i

allPositions :: Labyrinth -> [Position]
allPositions l = [Pos x y | x <- [0..w - 1], y <- [0..h - 1]]
    where w = labWidth l
          h = labHeight l

allCells :: Labyrinth -> [Cell]
allCells l = map (\p -> getP (cell p) l) $ allPositions l

allPosCells :: Labyrinth -> [(Position, Cell)]
allPosCells l = zipWith (,) (allPositions l) (allCells l)

pitCount :: Labyrinth -> Int
pitCount = length . filter (isPit . ctype_) . allCells
    where isPit (Pit _) = True
          isPit _       = False

pit :: Int -> Labyrinth -> Position
pit i = fst . fromJust . find (isIthPit . ctype_ . snd) . allPosCells
    where isIthPit (Pit j) = i == j
          isIthPit _       = False
