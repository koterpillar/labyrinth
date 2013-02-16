module Labyrinth.Generate (generateLabyrinth) where

import Labyrinth.Map

import Peeker

import Control.Monad.Random
import Control.Monad.State

generateLabyrinth :: (RandomGen g) => Int -> Int -> Int -> g -> (Labyrinth, g)
generateLabyrinth w h p = runRand (execStateT generate initial)
    where initial = emptyLabyrinth w h $ take p $ repeat $ Pos 0 0

isLand :: Cell -> Bool
isLand c = isLand' $ getP ctype c
    where isLand' Land = True
          isLand' _    = False

dotimes :: (Monad m) => Int -> m a -> m ()
dotimes n = sequence_ . replicate n

type LabGen g a = StateT Labyrinth (Rand g) a

chooseRandomR :: (RandomGen g) => [a] -> LabGen g a
chooseRandomR l = do
    if length l == 0
        then error "cannot generate anything!"
        else do
            i <- getRandomR (0, length l - 1)
            return $ l !! i

landCellIf :: (RandomGen g) => ((Position, Cell) -> Bool) -> LabGen g Position
landCellIf prop = do
    cells <- gets allPosCells
    let land = filter prop cells
    let goodLand = filter prop land
    (p, _) <- chooseRandomR goodLand
    return p

landCell :: (RandomGen g) => LabGen g Position
landCell = landCellIf (const True)

putCell :: (RandomGen g) => CellType -> LabGen g ()
putCell ct = do
    c <- landCellIf $ isLand . snd
    updS (cell c ~> ctype) ct

noTreasures :: Cell -> Bool
noTreasures = ([] ==) . getP ctreasures

putTreasure :: (RandomGen g) => Treasure -> LabGen g ()
putTreasure t = do
    c <- landCellIf $ (\c -> noTreasures c && isLand c) . snd
    updS (cell c ~> ctreasures) [t]

hasWall :: (RandomGen g) => (Position, Direction) -> LabGen g Bool
hasWall (p, d) = do
    wall <- getS (wall p d)
    return $ wall /= NoWall

putExit :: (RandomGen g) => Wall -> LabGen g ()
putExit w = do
    outer <- gets outerPos
    outer' <- filterM hasWall outer
    (p, d) <- chooseRandomR outer'
    updS (wall p d) w

putPit :: (RandomGen g) => Int -> LabGen g ()
putPit = putCell . Pit

generate :: (RandomGen g) => LabGen g ()
generate = do
    -- Put armories and hospitals in random places
    forM [Armory, Hospital] $ dotimes 2 . putCell
    -- Make exits
    let exits = 2
    dotimes exits $ putExit NoWall
    dotimes exits $ putExit Wall
    -- Pits
    w <- gets labWidth
    h <- gets labHeight
    let pits = (w + h) `div` 2
    forM [0..pits - 1] $ putPit
    -- Put treasures
    putTreasure TrueTreasure
    pc <- gets playerCount
    fakeTreasures <- getRandomR (1, pc)
    dotimes fakeTreasures $ putTreasure FakeTreasure
    -- TODO: rivers
    -- TODO: reachability
    return ()
