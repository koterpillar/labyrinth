module Labyrinth.Generate where

import Labyrinth.Map
import Labyrinth.Reachability

import Control.Lens hiding (allOf)
import Control.Monad.Loops
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State

import Data.Functor.Identity
import qualified Data.Map as M
import Data.Maybe
import Data.Tuple

generateLabyrinth :: RandomGen g => Int -> Int -> Int -> g -> (Labyrinth, g)
generateLabyrinth w h p = runRand $ execStateT generate $ emptyLabyrinth w h p

type LabState m a = StateT Labyrinth m a

type LabGen g a = LabState (Rand g) a

type CellPredicate m = Position -> LabState m Bool

type CellPredicateR g = CellPredicate (Rand g)

isTypeF :: Monad m => (CellType -> Bool) -> CellPredicate m
isTypeF prop pos = do
    ct <- use (cell pos . ctype)
    return $ prop ct

isType :: Monad m => CellType -> CellPredicate m
isType ct = isTypeF (ct ==)

isLand :: Monad m => CellPredicate m
isLand = isType Land

dotimes :: Monad m => Int -> m a -> m ()
dotimes n = sequence_ . replicate n

perimeter :: Labyrinth -> Int
perimeter l = (l ^. labWidth + l ^. labHeight) * 2

area :: Labyrinth -> Int
area l = l ^. labWidth * l ^. labHeight

chooseRandomR :: RandomGen g => [a] -> LabGen g a
chooseRandomR l = do
    if length l == 0
        then error "cannot generate anything!"
        else do
            i <- getRandomR (0, length l - 1)
            return $ l !! i

randomDirection :: RandomGen g => LabGen g Direction
randomDirection = chooseRandomR allDirections

allOf :: Monad m => [a -> m Bool] -> a -> m Bool
allOf = flip $ \val -> liftM and . sequence . map ($ val)

cellIf :: RandomGen g => CellPredicateR g -> LabGen g Position
cellIf prop = do
    cells <- gets allPositions
    good <- filterM prop cells
    chooseRandomR good

putCell :: RandomGen g => CellType -> LabGen g Position
putCell = putCellIf (return . const True)

putCellIf :: RandomGen g => CellPredicateR g -> CellType -> LabGen g Position
putCellIf prop ct = do
    pos <- cellIf $ allOf [isLand, prop]
    cell pos . ctype .= ct
    return pos

neighbors :: Monad m => Position -> LabState m [Position]
neighbors p = filterM (gets . isInside) possibleNeighbors
    where possibleNeighbors = map (advance p) allDirections

allNeighbors :: Monad m => CellPredicate m -> CellPredicate m
allNeighbors prop pos = do
    neigh <- neighbors pos
    let neigh' = pos:neigh
    res <- mapM prop neigh'
    return $ and res

isArmoryHospital :: Monad m => CellPredicate m
isArmoryHospital = isTypeF isAH
    where isAH Armory   = True
          isAH Hospital = True
          isAH _        = False

putAH :: RandomGen g => CellType -> LabGen g Position
putAH ct = putCellIf noAHNearby ct
    where noAHNearby = allNeighbors $ liftM not . isArmoryHospital

putArmories :: RandomGen g => LabGen g ()
putArmories = dotimes 2 $ putAH Armory

putHospitals :: RandomGen g => LabGen g ()
putHospitals = dotimes 2 $ putAH Hospital

noTreasures :: Monad m => CellPredicate m
noTreasures pos = do
    treasures <- use (cell pos . ctreasures)
    return $ null treasures

putTreasure :: RandomGen g => Treasure -> LabGen g ()
putTreasure t = do
    pos <- cellIf $ allOf [isLand, noTreasures]
    cell pos . ctreasures .= [t]

hasWall :: Monad m => Direction -> CellPredicate m
hasWall d p = do
    wall <- use (wall p d)
    return $ wall /= NoWall

putExit :: RandomGen g => Wall -> LabGen g ()
putExit w = do
    outer <- gets outerPos
    outer' <- filterM (allNeighbors noTreasures . fst) outer
    outer'' <- filterM (uncurry hasWall . swap) outer
    (p, d) <- chooseRandomR outer''
    wall p d .= w

putExits :: RandomGen g => LabGen g ()
putExits = do
    p <- gets perimeter
    let exits = p `div` 10
    dotimes exits $ putExit NoWall
    dotimes exits $ putExit Wall

putPits :: RandomGen g => LabGen g ()
putPits = do
    p <- gets perimeter
    let pits = p `div` 4
    forM_ [0..pits - 1] $ putCell . Pit

foldTimes :: Monad m => a -> Int -> (a -> m a) -> m a
foldTimes init times func = foldM func' init [1..times]
    where func' x y = func x

foldTimes_ :: Monad m => a -> Int -> (a -> m a) -> m ()
foldTimes_ init times func = do
    foldTimes init times func
    return ()

putRivers :: RandomGen g => LabGen g ()
putRivers = do
    a <- gets area
    let deltas = a `div` 12
    dotimes deltas $ do
        delta <- putCellIf hasLandAround RiverDelta
        riverLen <- getRandomR (2, 5)
        foldTimes_ delta riverLen $ \p -> do
            landDirs <- filterM ((flip landCellThere) p) allDirections
            if null landDirs
                then return p
                else do
                    d <- chooseRandomR landDirs
                    let p2 = advance p d
                    cell p2 . ctype .= River (opposite d)
                    return p2

hasLandAround :: Monad m => CellPredicate m
hasLandAround pos = do
    haveLand <- mapM (\d -> landCellThere d pos) allDirections
    return $ or haveLand

landCellThere :: Monad m => Direction -> CellPredicate m
landCellThere d p = do
    let p2 = advance p d
    inside <- gets $ isInside p2
    if inside
        then isLand p2
        else return False

putTreasures :: RandomGen g => LabGen g ()
putTreasures = do
    putTreasure TrueTreasure
    pc <- gets playerCount
    fakeTreasures <- getRandomR (1, pc)
    dotimes fakeTreasures $ putTreasure FakeTreasure

readProp :: Monad m => (Position -> Reader Labyrinth Bool) -> CellPredicate m
readProp prop pos = gets $ runReader $ prop pos

putWalls :: RandomGen g => LabGen g ()
putWalls = do
        a <- gets area
        walls <- getRandomR (a `div` 4, a `div` 2)
        forM_ [1..walls] $ \_ -> do
            d <- randomDirection
            pos <- cellIf $ allOf $ map ($ d) [ notRiver
                                              , notToOutside
                                              ]
            wall pos d .= Wall
        return ()
    where
        notRiver dir pos = do
            ct1 <- use $ cell pos . ctype
            if ct1 == River dir
                then return False
                else do
                    let pos2 = advance pos dir
                    let dir2 = opposite dir
                    inside <- gets $ isInside pos2
                    if inside
                        then do
                            ct2 <- use $ cell pos2 . ctype
                            return $ ct2 /= River dir2
                        else return True
        notToOutside :: Monad m => Direction -> CellPredicate m
        notToOutside dir pos = do
            let pos2 = advance pos dir
            gets $ isInside pos2

goodReachability :: Monad m => LabState m Bool
goodReachability = gets $ runReader $ do
    n <- asks area
    r <- asks (reachConverge $ n `div` 3)
    pos <- asks allPositions
    let res = map (\p -> M.findWithDefault False p r) pos
    return $ and res

goodDistribution :: Monad m => LabState m Bool
goodDistribution = gets $ runReader $ do
    n <- asks area
    r <- asks (converge $ n * 2)
    let res = maximum $ M.elems r
    return $ res <= 0.15

untilR :: MonadState v m => m Bool -> m a -> m ()
untilR prop act = do
    v <- get
    untilM_ (put v >> act) prop

untilRN :: MonadState v m => Int -> m Bool -> m a -> m Bool
untilRN 0 _ _ = return False
untilRN n prop act = do
    v <- get
    act
    res <- prop
    if res
        then return True
        else do
            put v
            untilRN (n - 1) prop act

generate :: RandomGen g => LabGen g ()
generate = do
    untilRN 10 goodDistribution $ do
        res <- untilRN 10 goodReachability $ do
            putArmories
            putHospitals
            putPits
            untilRN 50 goodReachability $ do
                putRivers
                putWalls
        if res
            then do
                putTreasures
                putExits
            else error "cannot generate anything!"
    return ()
