module Labyrinth.Action where

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

performCellActions :: CellType -> Position -> Int -> State Labyrinth (Maybe Position)
performCellActions Land _ _ = return Nothing
performCellActions Armory _ pi = do
    updS (player pi ~> pbullets) maxBullets
    updS (player pi ~> pgrenades) maxGrenades
    return Nothing

performAction :: Action -> State Labyrinth ActionResult
performAction (Go (Towards dir)) = do
    pi <- getS currentPlayer
    pos <- getS (player pi ~> position)
    w <- getS (wall pos dir)
    if w == NoWall then do
        let npos = advance pos dir
        updS (player pi ~> position) npos
        ct <- getS (cell npos ~> ctype)
        -- Perform cell-type-specific actions
        npos' <- performCellActions ct npos pi
        let npos'' = fromMaybe npos npos'
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
        if and [ptr == Nothing, length ctr > 0] then do
            let ctr' = tail ctr
            let ptr' = Just $ head ctr
            updS (cell npos'' ~> ctreasures) ctr'
            updS (player pi ~> ptreasure) ptr'
        else
            return ()
        return $ GoR $ Went ct cb cg (length ctr) nct
    else
        return $ GoR HitWall

performAction (Grenade dir) = do
    pi <- getS currentPlayer
    g <- getS (player pi ~> pgrenades)
    if g > 0 then do
        updS (player pi ~> pgrenades) (g - 1)
        pos <- getS (player pi ~> position)
        w <- getS (wall pos dir)
        if w /= HardWall then do
            updS (wall pos dir) NoWall
        else return ()
        return $ GrenadeR GrenadeOK
    else
        return $ GrenadeR NoGrenades
