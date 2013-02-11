module Labyrinth.Action (performMove) where

import Control.Monad.State

import Data.Maybe

import Labyrinth.Map
import Labyrinth.Move

import Peeker

performMove :: Move -> State Labyrinth MoveResult
performMove (Move actions) = do
    actionRes <- forM actions performAction
    current <- getS currentPlayer
    pCount <- gets playerCount
    let next = (current + 1) `mod` pCount
    updS currentPlayer next
    return $ MoveRes actionRes

transferAmmo :: Int -> Peek Labyrinth Int -> Peek Labyrinth Int -> State Labyrinth Int
transferAmmo maxAmount from to = do
    found <- getS from
    has <- getS to
    let amount = min found $ maxAmount - has
    let found' = found - amount
    let has' = has + amount
    updS from found'
    updS to has'
    return found

afterMove :: CellType -> Position -> Int -> State Labyrinth (Maybe Position)
afterMove Land _ _ = return Nothing
afterMove Armory _ pi = do
    updS (player pi ~> pbullets) maxBullets
    updS (player pi ~> pgrenades) maxGrenades
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

performAction :: Action -> State Labyrinth ActionResult
performAction (Go (Towards dir)) = do
    pi <- getS currentPlayer
    pos <- getS (player pi ~> position)
    w <- getS (wall pos dir)
    if w == NoWall
        then do
            let npos = advance pos dir
            updS (player pi ~> position) npos
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
            cb <- transferAmmo maxBullets
                (cell npos'' ~> cbullets)
                (player pi ~> pbullets)
            cg <- transferAmmo maxGrenades
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
            return $ GoR $ Went (ctResult ct) cb cg (length ctr) nctr
        else
            return $ GoR HitWall

performAction (Grenade dir) = do
    pi <- getS currentPlayer
    g <- getS (player pi ~> pgrenades)
    if g > 0
        then do
            updS (player pi ~> pgrenades) (g - 1)
            pos <- getS (player pi ~> position)
            w <- getS (wall pos dir)
            if w /= HardWall
                then do
                    updS (wall pos dir) NoWall
                else
                    return ()
            return $ GrenadeR GrenadeOK
        else
            return $ GrenadeR NoGrenades
