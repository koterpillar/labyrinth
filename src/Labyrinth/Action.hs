module Labyrinth.Action where

import Control.Monad.State

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

transferAmmo :: Int -> (Int, Int) -> (Int, Int)
transferAmmo maxAmount (has, found) = (has + amount, found - amount)
    where amount = min found $ maxAmount - has

transferAmmoFromTo :: Int -> Peek Labyrinth Int -> Peek Labyrinth Int -> State Labyrinth Int
transferAmmoFromTo maxAmount from to = do
    found <- getS from
    has <- getS to
    let (has', found') = transferAmmo maxAmount (has, found)
    updS from found'
    updS to has'
    return found

performAction :: Action -> State Labyrinth ActionResult
performAction (Go (Towards dir)) = do
    pi <- getS currentPlayer
    pos <- getS (player pi ~> position)
    w <- getS (wall pos dir)
    if w == NoWall then do
        let npos = advance pos dir
        updS (player pi ~> position) npos
        ct <- getS (cell npos ~> ctype)
        cb <- transferAmmoFromTo maxBullets
            (cell npos ~> cbullets)
            (player pi ~> pbullets)
        cg <- transferAmmoFromTo maxGrenades
            (cell npos ~> cgrenades)
            (player pi ~> pgrenades)
        return $ GoR $ Went ct cb cg 0 Nothing
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
