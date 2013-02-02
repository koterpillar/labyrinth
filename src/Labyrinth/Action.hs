module Labyrinth.Action where

import Control.Monad.State

import Labyrinth.Map
import Labyrinth.Move

import Peeker

performMove :: Move -> State Labyrinth MoveResult
performMove (Move []) = do
    l <- get
    current <- getS currentPlayer
    let pCount = playerCount l
    let next = (current + 1) `mod` pCount
    updS currentPlayer next
    return $ MoveRes []
performMove (Move (act:acts)) = do
    ar <- performAction act
    (MoveRes ars) <- performMove $ Move acts
    return $ MoveRes $ ar:ars

performAction :: Action -> State Labyrinth ActionResult
performAction (Go dir) = do
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

performAction (Grenade Next) = return $ ActionError "Cannot throw grenades in that direction."
performAction (Grenade dir) = do
    pi <- getS currentPlayer
    pos <- getS (player pi ~> position)
    w <- getS (wall pos dir)
    if w /= HardWall then do
        updS (wall pos dir) NoWall
    else return ()
    return $ GrenadeR GrenadeOK

