module Labyrinth.Generate (generateLabyrinth) where

import Labyrinth.Map

import Data.Tuple

import Peeker

import Control.Monad.Random
import Control.Monad.State

generateLabyrinth :: (RandomGen g) => Int -> Int -> Int -> g -> (Labyrinth, g)
generateLabyrinth w h p = runRand $ execStateT generate $ emptyLabyrinth w h p

type LabState m a = StateT Labyrinth m a

type LabGen g a = LabState (Rand g) a

type CellPredicate m = Position -> LabState m Bool

type CellPredicateR g = CellPredicate (Rand g)

allDirections :: [Direction]
allDirections = [L, R, U, D]

isTypeF :: (Monad m) => (CellType -> Bool) -> CellPredicate m
isTypeF prop pos = do
    ct <- getS (cell pos ~> ctype)
    return $ prop ct

isType :: (Monad m) => CellType -> CellPredicate m
isType ct = isTypeF (ct ==)

isLand :: (Monad m) => CellPredicate m
isLand = isType Land

dotimes :: (Monad m) => Int -> m a -> m ()
dotimes n = sequence_ . replicate n

perimeter :: (Monad m) => LabState m Int
perimeter = do
    w <- gets labWidth
    h <- gets labHeight
    return $ (w + h) * 2

area :: (Monad m) => LabState m Int
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

allOf :: (Monad m) => [a -> m Bool] -> a -> m Bool
allOf = flip $ \val -> (liftM and) . sequence . map ($ val)

cellIf :: (RandomGen g) => CellPredicateR g -> LabGen g Position
cellIf prop = do
    cells <- gets allPositions
    good <- filterM prop cells
    chooseRandomR good

putCell :: (RandomGen g) => CellType -> LabGen g Position
putCell = putCellIf (return . const True)

putCellIf :: (RandomGen g) => CellPredicateR g -> CellType -> LabGen g Position
putCellIf prop ct = do
    pos <- cellIf $ allOf [isLand, prop]
    updS (cell pos ~> ctype) ct
    return pos

neighbors :: (Monad m) => Position -> LabState m [Position]
neighbors p = filterM (gets . isInside) possibleNeighbors
    where possibleNeighbors = map (advance p) allDirections

allNeighbors :: (Monad m) => CellPredicate m -> CellPredicate m
allNeighbors prop pos = do
    neigh <- neighbors pos
    let neigh' = pos:neigh
    res <- mapM prop neigh'
    return $ and res

isArmoryHospital :: (Monad m) => CellPredicate m
isArmoryHospital = isTypeF isAH
    where isAH Armory   = True
          isAH Hospital = True
          isAH _        = False

putAH :: (RandomGen g) => CellType -> LabGen g Position
putAH ct = putCellIf noAHNearby ct
    where noAHNearby = allNeighbors $ liftM not . isArmoryHospital

putArmories :: (RandomGen g) => LabGen g ()
putArmories = dotimes 2 $ putAH Armory

putHospitals :: (RandomGen g) => LabGen g ()
putHospitals = dotimes 2 $ putAH Hospital

noTreasures :: (Monad m) => CellPredicate m
noTreasures pos = do
    treasures <- getS (cell pos ~> ctreasures)
    return $ null treasures

putTreasure :: (RandomGen g) => Treasure -> LabGen g ()
putTreasure t = do
    pos <- cellIf $ allOf [isLand, noTreasures]
    updS (cell pos ~> ctreasures) [t]

hasWall :: (Monad m) => Direction -> CellPredicate m
hasWall d p = do
    wall <- getS (wall p d)
    return $ wall /= NoWall

putExit :: (RandomGen g) => Wall -> LabGen g ()
putExit w = do
    outer <- gets outerPos
    outer' <- filterM (uncurry hasWall) $ map swap outer
    (d, p) <- chooseRandomR outer'
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
            landDirs <- filterM ((flip landCellThere) p) allDirections
            if null landDirs
                then return p
                else do
                    d <- chooseRandomR landDirs
                    let p2 = advance p d
                    updS (cell p2 ~> ctype) $ River $ opposite d
                    return p2

landCellThere :: (Monad m) => Direction -> CellPredicate m
landCellThere d p = do
    let p2 = advance p d
    inside <- gets $ isInside p2
    if inside
        then isLand p2
        else return False

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
            pos <- cellIf $ allOf $ map ($ d) [noWall, notRiver]
            updS (wall pos d) Wall
        return ()
    where
        noWall dir pos = liftM (== NoWall) $ getS $ wall pos dir
        notRiver dir pos = do
            ct1 <- getS $ cell pos ~> ctype
            if ct1 == River dir
                then return False
                else do
                    let pos2 = advance pos dir
                    let dir2 = opposite dir
                    inside <- gets $ isInside pos2
                    if inside
                        then do
                            ct2 <- getS $ cell pos2 ~> ctype
                            return $ ct2 /= River dir2
                        else return True

generate :: (RandomGen g) => LabGen g ()
generate = do
    putArmories
    putHospitals
    putRivers
    putPits
    putTreasures
    putExits
    putWalls
     -- TODO: reachability
    return ()
