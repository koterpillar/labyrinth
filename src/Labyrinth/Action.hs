module Labyrinth.Action where

import Control.Monad.State

import Labyrinth.Map
import Labyrinth.Move

import Peeker

performMove :: Move -> State Labyrinth MoveResult
performMove (Move actions) = do
    actionRes <- forM actions performAction
    l <- get
    current <- getS currentPlayer
    let pCount = playerCount l
    let next = (current + 1) `mod` pCount
    updS currentPlayer next
    return $ MoveRes actionRes

performAction :: Action -> State Labyrinth ActionResult
performAction (Go (Towards dir)) = do
    pi <- getS currentPlayer
    pos <- getS (player pi ~> position)
    w <- getS (wall pos dir)
    if w == NoWall then do
        let npos = advance pos dir
        updS (player pi ~> position) npos
        nc <- getS (cell npos ~> ctype)
        return $ GoR $ WentOnto nc
    else
        return $ GoR HitWall

performAction (Grenade dir) = do
    pi <- getS currentPlayer
    g <- getS (player pi ~> grenades)
    if g > 0 then do
        updS (player pi ~> grenades) (g - 1)
        pos <- getS (player pi ~> position)
        w <- getS (wall pos dir)
        if w /= HardWall then do
            updS (wall pos dir) NoWall
        else return ()
        return $ GrenadeR GrenadeOK
    else
        return $ GrenadeR NoGrenades
