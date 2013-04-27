{-# Language Rank2Types #-}

module Labyrinth.Action where

import Control.Lens hiding (Action)
import Control.Monad.State

import Data.List
import Data.Maybe
import Data.Tuple

import Labyrinth.Common
import Labyrinth.Map
import Labyrinth.Move
import Labyrinth.Show

type ActionState a = LabState (State [ActionResult]) a

putResult :: ActionResult -> ActionState ()
putResult r = lift $ modify (++[r])

matchResult :: String -> ActionState Bool
matchResult str = lift $ gets $ any (isInfixOf str . show)

returnContinue :: [Action] -> ActionResult -> ActionState ()
returnContinue rest res = do
    putResult res
    performActions rest

alwaysContinue :: [Action] -> ActionState ActionResult -> ActionState ()
alwaysContinue rest act = do
    res <- act
    returnContinue rest res

performMove :: PlayerId -> Move -> State Labyrinth MoveResult
performMove pi move = do
    res <- state $ \s -> swap $ runState (execStateT (performMove' pi move) s) []
    return $ MoveRes res

onlyWhenCurrent :: PlayerId -> ActionState () -> ActionState ()
onlyWhenCurrent pi act = do
    ended <- use gameEnded
    if ended
        then putResult WrongTurn
        else do
            current <- use currentTurn
            if current /= pi
                then putResult WrongTurn
                else act

onlyWhenChosen :: ActionState () -> ActionState ()
onlyWhenChosen act = do
    posChosen <- use positionsChosen
    if not posChosen
        then putResult InvalidMove
        else act

performMove' :: PlayerId -> Move -> ActionState ()
performMove' pi (Move actions) = onlyWhenCurrent pi $ onlyWhenChosen $
    if length (filter isMovement actions) > 1
        then putResult InvalidMove
        else do
            currentPlayer . pfell .= False
            performActions actions
            next <- advancePlayer
            case next of
                (Just pi) -> do
                    fallen <- use $ player pi . pfell
                    when fallen $ putResult $ WoundedAlert pi
                Nothing -> do
                    gameEnded .= True
                    putResult Draw

performMove' pi (ChoosePosition pos) = onlyWhenCurrent pi $ do
    out <- gets (isOutside pos)
    posChosen <- use positionsChosen
    if out || posChosen
        then putResult InvalidMove
        else do
            currentPlayer . position .= pos
            (Just next) <- advancePlayer
            if next == 0
                then do
                    positionsChosen .= True
                    players <- alivePlayers
                    pos <- forM players $ \pi -> do
                        (ct, cr) <- cellActions True
                        advancePlayer
                        return $ StartR pi ct cr
                    putResult $ GameStarted pos
                else
                    putResult $ ChoosePositionR ChosenOK

performMove' pi (ReorderCell pos) = onlyWhenCurrent pi $ onlyWhenChosen $ do
    out <- gets (isOutside pos)
    if out
        then putResult InvalidMove
        else do
            fell <- use $ currentPlayer . pfell
            if not fell
                then putResult InvalidMove
                else do
                    currentPlayer . position .= pos
                    currentPlayer . pfell .= False
                    (ct, cr) <- cellActions True
                    putResult $ ReorderCellR $ ReorderOK ct cr

performMove' pi (Query queries) = onlyWhenChosen $ performQueries pi queries

performMove' _ (Say _) = return ()

advancePlayer :: ActionState (Maybe PlayerId)
advancePlayer = do
    alive <- alivePlayers
    if null alive
        then return Nothing
        else do
            pi <- use currentTurn
            players <- allPlayers
            -- build a long enough queue starting with the next player
            let queue = take (length players) $ tail $ dropWhile (pi /=) $ cycle players
            -- select the first alive player from the queue
            next:_ <- filterM playerAlive queue
            currentTurn .= next
            return $ Just next

isMovement :: Action -> Bool
isMovement (Go _) = True
isMovement _ = False

performActions :: [Action] -> ActionState ()
performActions [] = return ()
performActions (act:rest) = case act of
    Go dir               -> performMovement dir rest
    Grenade dir          -> alwaysContinue rest $ performGrenade dir
    Shoot dir            -> alwaysContinue rest $ performShoot dir
    Surrender            -> performSurrender
    cond@(Conditional{}) -> performConditional cond rest

type AmmoLocation = Simple Lens Labyrinth Int

transferAmmo :: Maybe Int -> AmmoLocation -> AmmoLocation -> ActionState Int
transferAmmo maxAmount from to = do
    found <- use from
    has <- use to
    let amount = case maxAmount of
                     (Just max) -> min found $ max - has
                     Nothing    -> found
    let found' = found - amount
    let has' = has + amount
    from .= found'
    to .= has'
    return found

transferAmmo_ :: Maybe Int -> AmmoLocation -> AmmoLocation -> ActionState ()
transferAmmo_ maxAmount from to = do
    transferAmmo maxAmount from to
    return ()

pickBullets :: ActionState Int
pickBullets = do
    pos <- use $ currentPlayer . position
    out <- gets $ isOutside pos
    if out
        then return 0
        else transferAmmo
            (Just maxBullets)
            (cell pos . cbullets)
            (currentPlayer . pbullets)

pickGrenades :: ActionState Int
pickGrenades = do
    pos <- use $ currentPlayer . position
    out <- gets $ isOutside pos
    if out
        then return 0
        else transferAmmo
            (Just maxGrenades)
            (cell pos . cgrenades)
            (currentPlayer . pgrenades)

nextPit :: Monad m => Int -> LabState m Int
nextPit i = do
    npits <- gets pitCount
    return $ (i + 1) `mod` npits

cellActions :: Bool -> ActionState (CellTypeResult, CellEvents)
cellActions moved = do
    pos <- use (currentPlayer . position)
    ct <- use (cell pos . ctype)
    pos' <- case ct of
        Land -> return Nothing
        Armory -> do
            currentPlayer . pbullets .= maxBullets
            currentPlayer . pgrenades .= maxGrenades
            return Nothing
        Hospital -> do
            currentPlayer . phealth .= Healthy
            return Nothing
        Pit i -> if not moved then return Nothing else do
            i' <- nextPit i
            pos' <- gets (pit i')
            currentPlayer . position .= pos'
            return $ Just pos'
        River d -> do
            let pos' = advance pos d
            currentPlayer . position .= pos'
            return $ Just pos'
        RiverDelta -> return Nothing
    let npos = fromMaybe pos pos'
    -- If transported, determine the new cell type
    nct <- if isJust pos'
        then liftM Just $ use (cell npos . ctype)
        else return Nothing
    let nctr = fmap ctResult nct
    -- Pick ammo
    cb <- pickBullets
    cg <- pickGrenades
    -- Pick treasures
    ctr <- use (cell npos . ctreasures)
    ptr <- use (currentPlayer . ptreasure)
    when (isNothing ptr && length ctr > 0) $ do
        let ctr' = tail ctr
        let ptr' = Just $ head ctr
        cell npos . ctreasures .= ctr'
        currentPlayer . ptreasure .= ptr'
    return (ctResult ct, CellEvents cb cg (length ctr) nctr)

performMovement :: MoveDirection -> [Action] -> ActionState ()
performMovement (Towards dir) rest = let returnCont = returnContinue rest in do
    pos <- use (currentPlayer . position)
    let pos' = advance pos dir
    out <- gets $ isOutside pos
    out' <- gets $ isOutside pos'
    if out && out'
        then do
            currentPlayer . phealth .= Dead
            currentPlayer . pbullets .= 0
            currentPlayer . pgrenades .= 0
            putResult $ GoR LostOutside
        else do
            w <- use (wall pos dir)
            if w == NoWall
                then do
                    currentPlayer . position .= pos'
                    if out'
                        then do
                            t <- use (currentPlayer . ptreasure)
                            case t of
                                Nothing -> returnCont $ GoR $ WentOutside Nothing
                                (Just FakeTreasure) -> do
                                    currentPlayer . ptreasure .= Nothing
                                    returnCont $ GoR $ WentOutside $ Just TurnedToAshesR
                                (Just TrueTreasure) -> do
                                    currentPlayer . ptreasure .= Nothing
                                    gameEnded .= True
                                    putResult $ GoR $ WentOutside $ Just TrueTreasureR
                        else do
                            (ct, cr) <- cellActions True
                            returnCont $ GoR $ Went ct cr
                else do
                    (_, cr) <- cellActions False
                    returnCont $ GoR $ HitWall cr

performMovement Next rest = alwaysContinue rest $ liftM GoR $ do
    pos <- use (currentPlayer . position)
    out <- gets $ isOutside pos
    if out
        then return InvalidMovement
        else do
            ct <- use (cell pos . ctype)
            case ct of
                Pit _ -> do
                    (ct, cr) <- cellActions True
                    return $ Went ct cr
                River d -> do
                    (ct, cr) <- cellActions True
                    return $ Went ct cr
                _ -> return InvalidMovement

performGrenade :: Direction -> ActionState ActionResult
performGrenade dir = do
    g <- use (currentPlayer . pgrenades)
    if g > 0
        then do
            currentPlayer . pgrenades -= 1
            pickGrenades
            pos <- use (currentPlayer . position)
            out <- gets $ isOutside pos
            unless out $ do
                ct <- use (cell pos . ctype)
                when (ct == Armory) $
                    currentPlayer . pgrenades .= maxGrenades
                w <- use (wall pos dir)
                when (w /= HardWall) $
                    wall pos dir .= NoWall
            return $ GrenadeR GrenadeOK
        else
            return $ GrenadeR NoGrenades

performShoot :: Direction -> ActionState ActionResult
performShoot dir = do
    b <- use (currentPlayer . pbullets)
    if b > 0
        then do
            pos <- use (currentPlayer . position)
            ct <- use (cell pos . ctype)
            if ct == Hospital || ct == Armory
                then return $ ShootR Forbidden
                else do
                    currentPlayer . pbullets -= 1
                    pickBullets
                    res <- performShootFrom pos dir
                    return $ ShootR res
        else
            return $ ShootR NoBullets

allPlayers :: Monad m => LabState m [PlayerId]
allPlayers = do
    cnt <- gets playerCount
    return [0..cnt - 1]

alivePlayers :: Monad m => LabState m [PlayerId]
alivePlayers = do
    players <- allPlayers
    filterM playerAlive players

playerAlive :: Monad m => PlayerId -> LabState m Bool
playerAlive i = do
    ph <- use (player i . phealth)
    return $ ph /= Dead

playersAliveAt :: Monad m => Position -> LabState m [PlayerId]
playersAliveAt pos = do
    alive <- alivePlayers
    atPos <- filterM (playerAt pos) alive
    filterM notFallen atPos

playerAt :: Monad m => Position -> PlayerId -> LabState m Bool
playerAt pos i = do
    pp <- use (player i . position)
    return $ pos == pp

notFallen :: Monad m => PlayerId -> LabState m Bool
notFallen i = liftM not $ use (player i . pfell)

performShootFrom :: Position -> Direction -> ActionState ShootResult
performShootFrom pos dir = do
    outside <- gets $ isOutside pos
    ct <- use (cell pos . ctype)
    cnt <- gets playerCount
    hit <- playersAliveAt pos
    if not outside && ct == Hospital
        then return ShootOK
        else do
            pi <- use currentTurn
            let othersHit = delete pi hit
            if null othersHit
                then if outside || ct == Armory
                    then return ShootOK
                    else do
                        w <- use (wall pos dir)
                        if w == NoWall
                            then performShootFrom (advance pos dir) dir
                            else return ShootOK
                else do
                    forM_ othersHit $ \i -> do
                        ph <- use (player i . phealth)
                        dropBullets i
                        dropTreasure i
                        when (ph == Healthy) $ do
                            player i . phealth .= Wounded
                            player i . pfell .= True
                        when (ph == Wounded) $ do
                            dropGrenades i
                            player i . phealth .= Dead
                    return Scream

dropBullets :: PlayerId -> ActionState ()
dropBullets i = do
    pos <- use $ player i . position
    outside <- gets $ isOutside pos
    if outside
        then (player i . pbullets) .= 0
        else transferAmmo_ Nothing (player i . pbullets) (cell pos . cbullets)

dropGrenades :: PlayerId -> ActionState ()
dropGrenades i = do
    pos <- use $ player i . position
    outside <- gets $ isOutside pos
    if outside
        then (player i . pgrenades) .= 0
        else transferAmmo_ Nothing (player i . pgrenades) (cell pos . cgrenades)

dropTreasure :: PlayerId -> ActionState ()
dropTreasure i = do
    pos <- use $ player i . position
    outside <- gets $ isOutside pos
    tr <- (player i . ptreasure) <<.= Nothing
    unless outside $ case tr of
        Nothing -> return ()
        Just tr' -> (cell pos . ctreasures) %= (tr':)

performSurrender :: ActionState ()
performSurrender = do
    i <- use currentTurn
    dropBullets i
    dropGrenades i
    dropTreasure i
    player i . phealth .= Dead
    putResult Surrendered

performConditional :: Action -> [Action] -> ActionState ()
performConditional (Conditional cif cthen celse) rest = do
    match <- matchResult cif
    let branch = if match then cthen else celse
    performActions $ branch ++ rest

performQueries :: PlayerId -> [QueryType] -> ActionState ()
performQueries pi = mapM_ (performQuery pi)

performQuery :: PlayerId -> QueryType -> ActionState ()
performQuery pi q = do
    let p restype param = liftM restype $ use (player pi . param)
    qr <- case q of
        BulletCount     -> p BulletCountR                pbullets
        GrenadeCount    -> p GrenadeCountR               pgrenades
        PlayerHealth    -> p HealthR                     phealth
        TreasureCarried -> p (TreasureCarriedR . isJust) ptreasure
    putResult $ QueryR qr
