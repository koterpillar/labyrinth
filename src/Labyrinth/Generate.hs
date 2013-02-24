module Labyrinth.Generate (generateLabyrinth) where

import Labyrinth.Map

import Peeker

import Control.Monad.Random
import Control.Monad.State

generateLabyrinth :: (RandomGen g) => Int -> Int -> Int -> g -> (Labyrinth, g)
generateLabyrinth w h p = runRand $ execStateT generate $ emptyLabyrinth w h p

allDirections :: [Direction]
allDirections = [L, R, U, D]

isLand :: Cell -> Bool
isLand c = isLand' $ getP ctype c
    where isLand' Land = True
          isLand' _    = False

dotimes :: (Monad m) => Int -> m a -> m ()
dotimes n = sequence_ . replicate n

type LabGen g a = StateT Labyrinth (Rand g) a

perimeter :: (RandomGen g) => LabGen g Int
perimeter = do
    w <- gets labWidth
    h <- gets labHeight
    return $ (w + h) * 2

area :: (RandomGen g) => LabGen g Int
area = do
    w <- gets labWidth
    h <- gets labHeight
    return $ w * h

chooseRandomR :: (RandomGen g) => [a] -> LabGen g a
chooseRandomR l = do
    if length l == 0
        then error "cannot generate anything!"
        else do
            i <- getRandomR (0, length l - 1)
            return $ l !! i

randomDirection :: (RandomGen g) => LabGen g Direction
randomDirection = chooseRandomR allDirections

allOf :: [a -> Bool] -> a -> Bool
allOf = flip $ \val -> and . map ($ val)

allOfM :: (Monad m) => [a -> m Bool] -> a -> m Bool
allOfM = flip $ \val -> (liftM and) . sequence . map ($ val)

cellIfM :: (RandomGen g) => ((Position, Cell) -> LabGen g Bool) -> LabGen g (Position, Cell)
cellIfM prop = do
    cells <- gets allPosCells
    good <- filterM prop cells
    chooseRandomR good

cellIf :: (RandomGen g) => ((Position, Cell) -> Bool) -> LabGen g (Position, Cell)
cellIf prop = cellIfM $ return . prop

putCell :: (RandomGen g) => CellType -> LabGen g Position
putCell ct = do
    (c, _) <- cellIf $ allOf [isLand . snd]
    updS (cell c ~> ctype) ct
    return c

noTreasures :: Cell -> Bool
noTreasures = ([] ==) . getP ctreasures

putTreasure :: (RandomGen g) => Treasure -> LabGen g ()
putTreasure t = do
    (c, _) <- cellIf $ allOf $ map (. snd) [isLand, noTreasures]
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

putExits :: (RandomGen g) => LabGen g ()
putExits = do
    p <- perimeter
    let exits = p `div` 10
    dotimes exits $ putExit NoWall
    dotimes exits $ putExit Wall

putPits :: (RandomGen g) => LabGen g ()
putPits = do
    p <- perimeter
    let pits = p `div` 4
    forM_ [0..pits - 1] $ putCell . Pit

foldTimes :: (Monad m) => a -> Int -> (a -> m a) -> m a
foldTimes init times func = foldM func' init [1..times]
    where func' x y = func x

foldTimes_ :: (Monad m) => a -> Int -> (a -> m a) -> m ()
foldTimes_ init times func = do
    foldTimes init times func
    return ()

putRivers :: (RandomGen g) => LabGen g ()
putRivers = do
    a <- area
    let deltas = a `div` 12
    dotimes deltas $ do
        delta <- putCell RiverDelta
        riverLen <- getRandomR (2, 5)
        foldTimes_ delta riverLen $ \p -> do
            landDirs <- filterM (landCellThere p) allDirections
            if null landDirs
                then return p
                else do
                    d <- chooseRandomR landDirs
                    let p2 = advance p d
                    updS (cell p2 ~> ctype) $ River $ reverseDir d
                    return p2

landCellThere :: (RandomGen g) => Position -> Direction -> LabGen g Bool
landCellThere p d = do
    let p2 = advance p d
    inside <- gets $ isInside p2
    if inside
        then do
            c2 <- getS $ cell p2
            return $ isLand c2
        else return False

isRiverSystem :: Cell -> Bool
isRiverSystem c = isRiverSystem' $ getP ctype c
    where isRiverSystem' RiverDelta = True
          isRiverSystem' (River _)  = True
          isRiverSystem' _          = False

putTreasures :: (RandomGen g) => LabGen g ()
putTreasures = do
    putTreasure TrueTreasure
    pc <- gets playerCount
    fakeTreasures <- getRandomR (1, pc)
    dotimes fakeTreasures $ putTreasure FakeTreasure

putWalls :: (RandomGen g) => LabGen g ()
putWalls = do
        a <- area
        let walls = a `div` 6
        forM_ [1..walls] $ \_ -> do
            d <- randomDirection
            (c, _) <- cellIfM $ allOfM $ map ((. fst) . ($ d)) [noWall, notRiver]
            updS (wall c d) Wall
        return ()
    where
        noWall dir pos = liftM (== NoWall) $ getS $ wall pos dir
        notRiver dir pos = liftM (/= (River dir)) $ getS $ (cell pos ~> ctype)

generate :: (RandomGen g) => LabGen g ()
generate = do
    forM [Armory, Hospital] $ dotimes 2 . putCell
    putRivers
    putExits
    putPits
    putTreasures
    putWalls
     -- TODO: reachability
    return ()
