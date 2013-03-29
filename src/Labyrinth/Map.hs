{-# Language TemplateHaskell #-}
module Labyrinth.Map where

import Control.Lens
import Control.Monad
import Control.Monad.State

import Data.List
import Data.List.Lens
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid

data Direction = L | R | U | D
                 deriving (Eq)

allDirections :: [Direction]
allDirections = [L, R, U, D]

opposite :: Direction -> Direction
opposite L = R
opposite R = L
opposite U = D
opposite D = U

data CellType = Land
              | Armory
              | Hospital
              | Pit { _pitNumber :: Int }
              | River { _riverDirection :: Direction }
              | RiverDelta
              deriving (Eq)

makeLenses ''CellType

data Treasure = TrueTreasure | FakeTreasure
                deriving (Eq)

data Cell = Cell { _ctype      :: CellType
                 , _cbullets   :: Int
                 , _cgrenades  :: Int
                 , _ctreasures :: [Treasure]
                 }
                 deriving (Eq)

makeLenses ''Cell

emptyCell :: CellType -> Cell
emptyCell ct = Cell { _ctype      = ct
                    , _cbullets   = 0
                    , _cgrenades  = 0
                    , _ctreasures = []
                    }

data Wall = NoWall | Wall | HardWall
    deriving (Eq)

data Position = Pos { pX :: Int
                    , pY :: Int
                    }
                deriving (Eq)

instance Ord Position where
    (Pos x1 y1) `compare` (Pos x2 y2) =
        (y1 `compare` y2) `mappend` (x1 `compare` x2)

instance Show Position where
    show (Pos x y) = "(" ++ (show x) ++ ", " ++ (show y) ++ ")"

advance :: Position -> Direction -> Position
advance (Pos x y) L = Pos (x - 1) y
advance (Pos x y) U = Pos x (y - 1)
advance (Pos x y) R = Pos (x + 1) y
advance (Pos x y) D = Pos x (y + 1)

data Health = Healthy | Wounded | Dead
              deriving (Eq)

data Player = Player { _position  :: Position
                     , _phealth   :: Health
                     , _pbullets  :: Int
                     , _pgrenades :: Int
                     , _ptreasure :: Maybe Treasure
                     , _pfell     :: Bool
                     }
              deriving (Eq)

makeLenses ''Player

maxBullets :: Int
maxBullets = 3

maxGrenades :: Int
maxGrenades = 3

initialPlayer :: Position -> Player
initialPlayer pos = Player { _position  = pos
                           , _phealth   = Healthy
                           , _pbullets  = maxBullets
                           , _pgrenades = maxGrenades
                           , _ptreasure = Nothing
                           , _pfell     = False
                           }

type PlayerId = Int

-- wallsV and wallsH are considered to be to the left and top of the cells
data Labyrinth = Labyrinth { _labWidth        :: Int
                           , _labHeight       :: Int
                           , _cells           :: M.Map Position Cell
                           , _wallsH          :: M.Map Position Wall
                           , _wallsV          :: M.Map Position Wall
                           , _players         :: [Player]
                           , _currentTurn     :: PlayerId
                           , _positionsChosen :: Bool
                           , _gameEnded       :: Bool
                           }
                 deriving (Eq)

makeLenses ''Labyrinth

isInside :: Position -> Labyrinth -> Bool
isInside (Pos x y) l = and [ x >= 0
                            , x < w
                            , y >= 0
                            , y < h
                            ]
    where w = l ^. labWidth
          h = l ^. labHeight

isOutside :: Position -> Labyrinth -> Bool
isOutside p = not . isInside p

outerPos :: Labyrinth -> [(Position, Direction)]
outerPos l = concat [ [(Pos x 0, U)       | x <- [0..w - 1]]
                    , [(Pos x (h - 1), D) | x <- [0..w - 1]]
                    , [(Pos 0 y, L)       | y <- [0..h - 1]]
                    , [(Pos (w - 1) y, R) | y <- [0..h - 1]]
                    ]
    where w = l ^. labWidth
          h = l ^. labHeight

playerCount :: Labyrinth -> Int
playerCount = length . (^. players)

posRectangle :: Int -> Int -> [Position]
posRectangle w h = [Pos x y | y <- [0..h - 1], x <- [0..w - 1]]

mapRectangle :: a -> Int -> Int -> M.Map Position a
mapRectangle x w h = M.fromList $ zip (posRectangle w h) (repeat x)

emptyLabyrinth :: Int -> Int -> Int -> Labyrinth
emptyLabyrinth w h playerCount =
    let initialLab = Labyrinth { _labWidth           = w
                               , _labHeight          = h
                               , _cells              = mapRectangle (emptyCell Land) w h
                               , _wallsH             = mapRectangle NoWall w (h + 1)
                               , _wallsV             = mapRectangle NoWall (w + 1) h
                               , _players            = replicate playerCount $ initialPlayer $ Pos 0 0
                               , _currentTurn        = 0
                               , _positionsChosen    = False
                               , _gameEnded          = False
                               }
    in (flip execState) initialLab $ do
        forM_ [0..w - 1] $ \x -> wall (Pos x 0) U .= HardWall
        forM_ [0..w - 1] $ \x -> wall (Pos x (h - 1)) D .= HardWall
        forM_ [0..h - 1] $ \y -> wall (Pos 0 y) L .= HardWall
        forM_ [0..h - 1] $ \y -> wall (Pos (w - 1) y) R .= HardWall

cell :: Position -> Simple Lens Labyrinth Cell
cell p = cells . ix' p

wallH :: Position -> Simple Lens Labyrinth Wall
wallH p = wallsH . ix' p

wallV :: Position -> Simple Lens Labyrinth Wall
wallV p = wallsV . ix' p

wall :: Position -> Direction -> Simple Lens Labyrinth Wall
wall p U = wallH p
wall p L = wallV p
wall p D = wallH (advance p D)
wall p R = wallV (advance p R)

ix' i = singular $ ix i

player :: PlayerId -> Simple Lens Labyrinth Player
player i = players . ix' i

currentPlayer :: Simple Lens Labyrinth Player
currentPlayer f l = (player i) f l
    where i = l ^?! currentTurn

allPositions :: Labyrinth -> [Position]
allPositions l = posRectangle w h
    where w = l ^. labWidth
          h = l ^. labHeight

allCells :: Labyrinth -> [Cell]
allCells l = map (\p -> l ^?! cell p) $ allPositions l

allPosCells :: Labyrinth -> [(Position, Cell)]
allPosCells l = zipWith (,) (allPositions l) (allCells l)

pitCount :: Labyrinth -> Int
pitCount = length . filter (isPit . _ctype) . allCells

armories :: Labyrinth -> [Position]
armories = map fst . filter ((Armory ==) . _ctype . snd) . allPosCells

pits :: Labyrinth -> [Position]
pits = map fst . filter (isPit . _ctype . snd) . allPosCells

isPit :: CellType -> Bool
isPit (Pit _) = True
isPit _       = False

pit :: Int -> Labyrinth -> Position
pit i = fst . fromJust . find (isIthPit . _ctype . snd) . allPosCells
    where isIthPit (Pit j) = i == j
          isIthPit _       = False
