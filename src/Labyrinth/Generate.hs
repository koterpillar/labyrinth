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

landCell :: (RandomGen g) => LabGen g Position
landCell = do
    cells <- gets allPosCells
    let land = filter (isLand . snd) cells
    if land == []
        then error "cannot generate anything!"
        else do
            i <- getRandomR (0, length land - 1)
            return $ fst $ land !! i

generate :: (RandomGen g) => LabGen g ()
generate = do
    -- Put armories and hospitals in random places
    forM [Armory, Hospital] $ \ct -> sequence_ $ replicate 2 $ do
        c <- landCell
        updS (cell c ~> ctype) ct
    return ()
