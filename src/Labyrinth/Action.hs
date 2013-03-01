module Labyrinth.Action (performMove) where

import Control.Monad.State

import Data.List
import Data.Maybe

import Labyrinth.Map
import Labyrinth.Move

import Peeker

performMove :: PlayerId -> Move -> State Labyrinth MoveResult
performMove pi move = do
    ended <- getS gameEnded
    if ended
        then return WrongTurn
        else do
            current <- getS currentTurn
            if current /= pi
                then return WrongTurn
                else performMove' move

onlyWhenChosen :: State Labyrinth MoveResult -> State Labyrinth MoveResult
onlyWhenChosen act = do
    posChosen <- getS positionsChosen
    if not posChosen
        then return InvalidMove
        else act

performMove' :: Move -> State Labyrinth MoveResult
performMove' (Move actions) = onlyWhenChosen $ do
    if length (filter isMovement actions) > 1
        then return InvalidMove
        else do
            updS (currentPlayer ~> pfell) False
            actionRes <- performActions actions
            next <- advancePlayer
            if isJust next
                then return $ MoveRes actionRes
                else do
                    updS gameEnded True
                    return $ MoveRes $ actionRes ++ [Draw]

performMove' (ChoosePosition pos) = do
    out <- gets (isOutside pos)
    posChosen <- getS positionsChosen
    if out || posChosen
        then return InvalidMove
        else do
            updS (currentPlayer ~> position) pos
            (Just next) <- advancePlayer
            if (next == 0)
                then do
                    updS positionsChosen True
                    players <- alivePlayers
                    pos <- forM players $ \pi -> do
                        (ct, cr) <- cellActions True
                        advancePlayer
                        return $ StartR pi ct cr
                    return $ ChoosePositionR $ AllChosenOK pos
                else
                    return $ ChoosePositionR ChosenOK

performMove' (ReorderCell pos) = onlyWhenChosen $ do
    out <- gets (isOutside pos)
    if out
        then return InvalidMove
        else do
            fell <- getS (currentPlayer ~> pfell)
            if not fell
                then return InvalidMove
                else do
                    updS (currentPlayer ~> position) pos
                    updS (currentPlayer ~> pfell) False
                    (ct, cr) <- cellActions True
                    return $ ReorderCellR $ ReorderOK ct cr

advancePlayer :: State Labyrinth (Maybe PlayerId)
advancePlayer = do
    alive <- alivePlayers
    if null alive
        then return Nothing
        else do
            pi <- getS currentTurn
            players <- allPlayers
            -- build a long enough queue starting with the next player
            let queue = take (length players) $ tail $ dropWhile (pi /=) $ cycle players
            -- select the first alive player from the queue
            next:_ <- filterM playerAlive queue
            updS currentTurn next
            return $ Just next

isMovement :: Action -> Bool
isMovement (Go _) = True
isMovement _ = False

returnContinue :: [Action] -> ActionResult -> State Labyrinth [ActionResult]
returnContinue rest res = do
    restRes <- performActions rest
    return $ res:restRes

returnStop :: ActionResult -> State Labyrinth [ActionResult]
returnStop = return . (:[])

alwaysContinue :: [Action] -> State Labyrinth ActionResult -> State Labyrinth [ActionResult]
alwaysContinue rest act = do
    res <- act
    returnContinue rest res

performActions :: [Action] -> State Labyrinth [ActionResult]
performActions [] = return []
performActions (act:rest) = case act of
    Go dir      -> performMovement dir rest
    Grenade dir -> alwaysContinue rest $ performGrenade dir
    Shoot dir   -> alwaysContinue rest $ performShoot dir

transferAmmo :: Maybe Int -> Peek Labyrinth Int -> Peek Labyrinth Int -> State Labyrinth Int
transferAmmo maxAmount from to = do
    found <- getS from
    has <- getS to
    let amount = case maxAmount of
                     (Just max) -> min found $ max - has
                     Nothing    -> found
    let found' = found - amount
    let has' = has + amount
    updS from found'
    updS to has'
    return found

transferAmmo_ :: Maybe Int -> Peek Labyrinth Int -> Peek Labyrinth Int -> State Labyrinth ()
transferAmmo_ maxAmount from to = do
    transferAmmo maxAmount from to
    return ()

pickBullets :: State Labyrinth Int
pickBullets = do
    i <- getS currentTurn
    pos <- getS $ player i ~> position
    out <- gets $ isOutside pos
    if out
        then return 0
        else transferAmmo
            (Just maxBullets)
            (cell pos ~> cbullets)
            (player i ~> pbullets)

pickGrenades :: State Labyrinth Int
pickGrenades = do
    i <- getS currentTurn
    pos <- getS $ player i ~> position
    out <- gets $ isOutside pos
    if out
        then return 0
        else transferAmmo
            (Just maxGrenades)
            (cell pos ~> cgrenades)
            (player i ~> pgrenades)

nextPit :: Int -> State Labyrinth Int
nextPit i = do
    npits <- gets pitCount
    return $ (i + 1) `mod` npits

cellActions :: Bool -> State Labyrinth (CellTypeResult, CellEvents)
cellActions moved = do
    pos <- getS (currentPlayer ~> position)
    ct <- getS (cell pos ~> ctype)
    pos' <- case ct of
        Land -> return Nothing
        Armory -> do
            updS (currentPlayer ~> pbullets) maxBullets
            updS (currentPlayer ~> pgrenades) maxGrenades
            return Nothing
        Hospital -> do
            updS (currentPlayer ~> phealth) Healthy
            return Nothing
        Pit i -> if not moved then return Nothing else do
            i' <- nextPit i
            pos' <- gets (pit i')
            updS (currentPlayer ~> position) pos'
            return $ Just pos'
        River d -> do
            let pos' = advance pos d
            updS (currentPlayer ~> position) pos'
            return $ Just pos'
        RiverDelta -> return Nothing
    let npos = fromMaybe pos pos'
    -- If transported, determine the new cell type
    nct <- if isJust pos'
        then (liftM Just) $ getS (cell npos ~> ctype)
        else return Nothing
    let nctr = (fmap ctResult) nct
    -- Pick ammo
    cb <- pickBullets
    cg <- pickGrenades
    -- Pick treasures
    ctr <- getS (cell npos ~> ctreasures)
    ptr <- getS (currentPlayer ~> ptreasure)
    when (ptr == Nothing && length ctr > 0) $ do
        let ctr' = tail ctr
        let ptr' = Just $ head ctr
        updS (cell npos ~> ctreasures) ctr'
        updS (currentPlayer ~> ptreasure) ptr'
    return $ (ctResult ct, CellEvents cb cg (length ctr) nctr)

performMovement :: MoveDirection -> [Action] -> State Labyrinth [ActionResult]
performMovement (Towards dir) rest = let returnCont = returnContinue rest in do
    pos <- getS (currentPlayer ~> position)
    let pos' = advance pos dir
    out <- gets $ isOutside pos
    out' <- gets $ isOutside pos'
    if out && out'
        then do
            updS (currentPlayer ~> phealth) Dead
            updS (currentPlayer ~> pbullets) 0
            updS (currentPlayer ~> pgrenades) 0
            returnStop $ GoR LostOutside
        else do
            w <- getS (wall pos dir)
            if w == NoWall
                then do
                    updS (currentPlayer ~> position) pos'
                    if out'
                        then do
                            t <- getS (currentPlayer ~> ptreasure)
                            case t of
                                Nothing -> returnCont $ GoR $ WentOutside Nothing
                                (Just FakeTreasure) -> do
                                    updS (currentPlayer ~> ptreasure) Nothing
                                    returnCont $ GoR $ WentOutside $ Just TurnedToAshesR
                                (Just TrueTreasure) -> do
                                    updS (currentPlayer ~> ptreasure) Nothing
                                    updS gameEnded True
                                    returnStop $ GoR $ WentOutside $ Just TrueTreasureR
                        else do
                            (ct, cr) <- cellActions True
                            returnCont $ GoR $ Went ct cr
                else do
                    (_, cr) <- cellActions False
                    returnCont $ GoR $ HitWall cr

performMovement Next rest = alwaysContinue rest $ liftM GoR $ do
    pos <- getS (currentPlayer ~> position)
    out <- gets $ isOutside pos
    if out
        then return InvalidMovement
        else do
            ct <- getS (cell pos ~> ctype)
            case ct of
                Pit _ -> do
                    (ct, cr) <- cellActions True
                    return $ Went ct cr
                River d -> do
                    (ct, cr) <- cellActions True
                    return $ Went ct cr
                _ -> return InvalidMovement

performGrenade :: Direction -> State Labyrinth ActionResult
performGrenade dir = do
    g <- getS (currentPlayer ~> pgrenades)
    if g > 0
        then do
            updS (currentPlayer ~> pgrenades) (g - 1)
            pickGrenades
            pos <- getS (currentPlayer ~> position)
            out <- gets $ isOutside pos
            if out then return ()
                else do
                    ct <- getS (cell pos ~> ctype)
                    when (ct == Armory) $
                        updS (currentPlayer ~> pgrenades) maxGrenades
                    w <- getS (wall pos dir)
                    if w /= HardWall
                        then do
                            updS (wall pos dir) NoWall
                        else
                            return ()
            return $ GrenadeR GrenadeOK
        else
            return $ GrenadeR NoGrenades

performShoot :: Direction -> State Labyrinth ActionResult
performShoot dir = do
    b <- getS (currentPlayer ~> pbullets)
    if b > 0
        then do
            pos <- getS (currentPlayer ~> position)
            ct <- getS (cell pos ~> ctype)
            if ct == Hospital || ct == Armory
                then return $ ShootR Forbidden
                else do
                    updS (currentPlayer ~> pbullets) (b - 1)
                    pickBullets
                    res <- performShootFrom pos dir
                    return $ ShootR res
        else
            return $ ShootR NoBullets

allPlayers :: State Labyrinth [PlayerId]
allPlayers = do
    cnt <- gets playerCount
    return $ [0..cnt - 1]

alivePlayers :: State Labyrinth [PlayerId]
alivePlayers = do
    players <- allPlayers
    filterM playerAlive players

playerAlive :: PlayerId -> State Labyrinth Bool
playerAlive i = do
    ph <- getS (player i ~> phealth)
    return $ ph /= Dead

playersAliveAt :: Position -> State Labyrinth [PlayerId]
playersAliveAt pos = do
    alive <- alivePlayers
    atPos <- filterM (playerAt pos) alive
    filterM notFallen atPos

playerAt :: Position -> PlayerId -> State Labyrinth Bool
playerAt pos i = do
    pp <- getS (player i ~> position)
    return $ pos == pp

notFallen :: PlayerId -> State Labyrinth Bool
notFallen i = (liftM not) $ getS (player i ~> pfell)

performShootFrom :: Position -> Direction -> State Labyrinth ShootResult
performShootFrom pos dir = do
    outside <- gets $ isOutside pos
    ct <- getS (cell pos ~> ctype)
    cnt <- gets playerCount
    hit <- playersAliveAt pos
    if not outside && ct == Hospital
        then return ShootOK
        else do
            pi <- getS currentTurn
            let othersHit = delete pi hit
            if length othersHit == 0
                then if outside
                    then return ShootOK
                    else do
                        if ct == Armory
                            then return ShootOK
                            else do
                                w <- getS (wall pos dir)
                                if w == NoWall
                                    then performShootFrom (advance pos dir) dir
                                    else return ShootOK
                else do
                    forM_ othersHit $ \i -> do
                        ph <- getS (player i ~> phealth)
                        if outside
                            then updS (player i ~> pbullets) 0
                            else transferAmmo_ Nothing (player i ~> pbullets) (cell pos ~> cbullets)
                        when (ph == Healthy) $ do
                            updS (player i ~> phealth) Wounded
                            updS (player i ~> pfell) True
                        when (ph == Wounded) $ do
                            if outside
                                then updS (player i ~> pgrenades) 0
                                else transferAmmo_ Nothing (player i ~> pgrenades) (cell pos ~> cgrenades)
                            updS (player i ~> phealth) Dead
                    return Scream
