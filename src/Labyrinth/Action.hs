module Labyrinth.Action (performMove) where

import Control.Monad.State

import Data.List
import Data.Maybe

import Labyrinth.Map
import Labyrinth.Move

import Peeker

performMove :: PlayerId -> Move -> State Labyrinth MoveResult
performMove pi (Move actions) = do
    current <- getS currentPlayer
    if current /= pi
        then return WrongTurn
        else if length (filter isMovement actions) > 1
            then return InvalidMove
            else do
                actionRes <- performActions actions
                pCount <- gets playerCount
                let next = (current + 1) `mod` pCount
                updS currentPlayer next
                return $ MoveRes actionRes

isMovement :: Action -> Bool
isMovement (Go _) = True
isMovement _ = False

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

afterMove :: CellType -> Position -> PlayerId -> State Labyrinth (Maybe Position)
afterMove Land _ _ = return Nothing
afterMove Armory _ pi = do
    updS (player pi ~> pbullets) maxBullets
    updS (player pi ~> pgrenades) maxGrenades
    return Nothing
afterMove Hospital _ pi = do
    updS (player pi ~> phealth) Healthy
    return Nothing
afterMove (Pit i) _ pi = do
    npits <- gets pitCount
    let i' = (i + 1) `mod` npits
    npos <- gets (pit i')
    return $ Just npos
afterMove (River d) npos pi = do
    let npos' = advance npos d
    return $ Just npos'
afterMove RiverDelta _ _ = return Nothing

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

performActions (Go (Towards dir):rest) = let returnCont = returnContinue rest in do
    pi <- getS currentPlayer
    pos <- getS (player pi ~> position)
    w <- getS (wall pos dir)
    if w == NoWall
        then do
            let npos = advance pos dir
            updS (player pi ~> position) npos
            out <- gets $ isOutside npos
            if out
                then do
                    t <- getS (player pi ~> ptreasure)
                    case t of
                        Nothing -> returnCont $ GoR $ WentOutside Nothing
                        (Just FakeTreasure) -> do
                            updS (player pi ~> ptreasure) Nothing
                            returnCont $ GoR $ WentOutside $ Just TurnedToAshesR
                        (Just TrueTreasure) -> do
                            updS (player pi ~> ptreasure) Nothing
                            returnStop $ GoR $ WentOutside $ Just TrueTreasureR
                            -- TODO: mark the game as ended?
                else do
                    ct <- getS (cell npos ~> ctype)
                    -- Perform cell-type-specific actions
                    npos' <- afterMove ct npos pi
                    let npos'' = fromMaybe npos npos'
                    updS (player pi ~> position) npos''
                    -- If transported, determine the new cell type
                    nct <- if isJust npos' then do
                            nct' <- getS (cell npos'' ~> ctype)
                            return $ Just nct'
                        else
                            return Nothing
                    -- Pick ammo
                    cb <- transferAmmo (Just maxBullets)
                        (cell npos'' ~> cbullets)
                        (player pi ~> pbullets)
                    cg <- transferAmmo (Just maxGrenades)
                        (cell npos'' ~> cgrenades)
                        (player pi ~> pgrenades)
                    -- Pick treasures
                    ctr <- getS (cell npos'' ~> ctreasures)
                    ptr <- getS (player pi ~> ptreasure)
                    if and [ptr == Nothing, length ctr > 0]
                        then do
                            let ctr' = tail ctr
                            let ptr' = Just $ head ctr
                            updS (cell npos'' ~> ctreasures) ctr'
                            updS (player pi ~> ptreasure) ptr'
                        else
                            return ()
                    let nctr = (fmap ctResult) nct
                    returnCont $ GoR $ Went (ctResult ct) cb cg (length ctr) nctr
        else
            returnCont $ GoR HitWall

performActions (Grenade dir:rest) = alwaysContinue rest $ do
    pi <- getS currentPlayer
    g <- getS (player pi ~> pgrenades)
    if g > 0
        then do
            updS (player pi ~> pgrenades) (g - 1)
            pos <- getS (player pi ~> position)
            out <- gets $ isOutside pos
            if out then return ()
                else do
                    w <- getS (wall pos dir)
                    if w /= HardWall
                        then do
                            updS (wall pos dir) NoWall
                        else
                            return ()
            return $ GrenadeR GrenadeOK
        else
            return $ GrenadeR NoGrenades

performActions (Shoot dir:rest) = alwaysContinue rest $ do
    pi <- getS currentPlayer
    b <- getS (player pi ~> pbullets)
    if b > 0
        then do
            updS (player pi ~> pbullets) (b - 1)
            pos <- getS (player pi ~> position)
            res <- performShoot pos dir
            return $ ShootR res
        else
            return $ ShootR NoBullets

playersAliveAt :: Position -> State Labyrinth [PlayerId]
playersAliveAt pos = do
    cnt <- gets playerCount
    let allPlayers = [0..cnt - 1]
    filterM (playerAliveAt pos) allPlayers

playerAliveAt :: Position -> PlayerId -> State Labyrinth Bool
playerAliveAt pos i = do
    pp <- getS (player i ~> position)
    ph <- getS (player i ~> phealth)
    return $ pos == pp && ph /= Dead

performShoot :: Position -> Direction -> State Labyrinth ShootResult
performShoot pos dir = do
    pi <- getS currentPlayer
    cnt <- gets playerCount
    hit <- playersAliveAt pos
    let othersHit = delete pi hit
    outside <- gets $ isOutside pos
    if length othersHit == 0
        then if outside
            then return ShootOK
            else do
                -- TODO: check for a wall
                performShoot (advance pos dir) dir
        else do
            forM_ othersHit $ \i -> do
                ph <- getS (player i ~> phealth)
                when (ph == Healthy) $ do
                    updS (player i ~> phealth) Wounded
                when (ph == Wounded) $ do
                    updS (player i ~> phealth) Dead
                transferAmmo Nothing (player i ~> pbullets) (cell pos ~> cbullets)
            return Scream
