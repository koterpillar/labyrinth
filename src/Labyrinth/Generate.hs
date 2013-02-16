module Labyrinth.Generate (generateLabyrinth) where

import Labyrinth.Map

import Peeker

import Control.Monad.Random
import Control.Monad.State

generateLabyrinth :: (RandomGen g) => Int -> Int -> Int -> g -> (Labyrinth, g)
generateLabyrinth w h p = runRand (execStateT generate initial)
    where initial = emptyLabyrinth w h $ take p $ repeat $ Pos 0 0

type LabGen g a = StateT Labyrinth (Rand g) a

isLand :: Cell -> Bool
isLand c = isLand' $ getP ctype c
    where isLand' Land = True
          isLand' _    = False

landCellIf :: (RandomGen g) => ((Position, Cell) -> Bool) -> LabGen g Position
landCellIf prop = do
    cells <- gets allPosCells
    let land = filter (isLand . snd) cells
    let goodLand = filter prop land
    if goodLand == []
        then error "cannot generate anything!"
        else do
            i <- getRandomR (0, length goodLand - 1)
            return $ fst $ goodLand !! i

landCell :: (RandomGen g) => LabGen g Position
landCell = landCellIf (const True)

putCell :: (RandomGen g) => CellType -> LabGen g ()
putCell ct = do
    c <- landCell
    updS (cell c ~> ctype) ct

putTreasure :: (RandomGen g) => Treasure -> LabGen g ()
putTreasure t = do
    c <- landCellIf (([] ==) . getP ctreasures . snd)
    updS (cell c ~> ctreasures) [t]

dotimes :: (Monad m) => Int -> m a -> m ()
dotimes n = sequence_ . replicate n

generate :: (RandomGen g) => LabGen g ()
generate = do
    -- Put armories and hospitals in random places
    forM [Armory, Hospital] $ dotimes 2 . putCell
    -- Put treasures
    putTreasure TrueTreasure
    pc <- gets playerCount
    fakeTreasures <- getRandomR (1, pc)
    dotimes fakeTreasures $ putTreasure FakeTreasure
    return ()
