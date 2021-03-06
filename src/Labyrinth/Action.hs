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
            currentPlayer . pjustShot .= False
            performActions actions
            next <- advancePlayer
            case next of
                (Just pi) -> do
                    justShot <- isFallen pi
                    when justShot $ putResult $ WoundedAlert pi Wounded
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
            fell <- use $ currentPlayer . pjustShot
            if not fell
                then putResult InvalidMove
                else do
                    currentPlayer . position .= pos
                    currentPlayer . pjustShot .= False
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
            let queue = tail $ dropWhile (pi /=) $ cycle players
            let advance (pi':ps) = do
                alive <- playerAlive pi'
                if alive
                    then return pi'
                    else do
                        justShot <- isFallen pi'
                        when justShot $ do
                            (player pi' . pjustShot) .= False
                            putResult $ WoundedAlert pi' Dead
                        advance ps
            next <- advance queue
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

pickInside :: ActionState Int -> ActionState Int
pickInside action = do
    pos <- use $ currentPlayer . position
    out <- gets $ isOutside pos
    if out
        then return 0
        else action

pickBullets :: ActionState Int
pickBullets = pickInside $ do
    pos <- use $ currentPlayer . position
    health <- use $ currentPlayer . phealth
    if health == Wounded
        then use $ cell pos . cbullets
        else transferAmmo
                (Just maxBullets)
                (cell pos . cbullets)
                (currentPlayer . pbullets)

pickGrenades :: ActionState Int
pickGrenades = pickInside $ do
    pos <- use $ currentPlayer . position
    transferAmmo
        (Just maxGrenades)
        (cell pos . cgrenades)
        (currentPlayer . pgrenades)

pickTreasures :: ActionState Int
pickTreasures = pickInside $ do
    pos <- use $ currentPlayer . position
    ctr <- use (cell pos . ctreasures)
    let nctr = length ctr
    health <- use $ currentPlayer . phealth
    when (health == Healthy) $ do
        ptr <- use (currentPlayer . ptreasure)
        when (isNothing ptr && not (null ctr)) $ do
            let ctr' = tail ctr
            let ptr' = Just $ head ctr
            cell pos . ctreasures .= ctr'
            currentPlayer . ptreasure .= ptr'
    return nctr

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
    let nctype = fmap ctResult nct
    cb <- pickBullets
    cg <- pickGrenades
    ctr <- pickTreasures
    return (ctResult ct, CellEvents cb cg ctr nctype)

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
            out <- gets $ isOutside pos
            ct <- use (cell pos . ctype)
            if not out && (ct == Hospital || ct == Armory)
                then return $ ShootR Forbidden
                else do
                    currentPlayer . pbullets -= 1
                    pickBullets
                    res <- performShootFrom pos dir
                    return $ ShootR res
        else
            return $ ShootR NoBullets

performShootFrom :: Position -> Direction -> ActionState ShootResult
performShootFrom pos dir =
    let endShot = return ShootOK
        endShotIf cond act = if cond then endShot else act
    in do
        wayOut <- gets $ wayOutside pos
        endShotIf wayOut $ do
            out <- gets $ isOutside pos
            ct <- use (cell pos . ctype)
            endShotIf (not out && ct == Hospital) $ do
                hit <- playersAliveAt pos
                pi <- use currentTurn
                let othersHit = delete pi hit
                if null othersHit
                    then endShotIf (not out && ct == Armory) $ do
                        let npos = advance pos dir
                        nout <- gets $ isOutside npos
                        w <- use (wall pos dir)
                        if (out && nout) || w == NoWall
                            then performShootFrom (advance pos dir) dir
                            else endShot
                    else do
                        forM_ othersHit $ \i -> do
                            ph <- use (player i . phealth)
                            dropBullets i
                            dropTreasure i
                            when (ph == Wounded) $ dropGrenades i
                            player i . pjustShot .= True
                            player i . phealth %= pred
                        return Scream

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

isFallen :: Monad m => PlayerId -> LabState m Bool
isFallen i = use (player i . pjustShot)

notFallen :: Monad m => PlayerId -> LabState m Bool
notFallen i = liftM not $ isFallen i

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
