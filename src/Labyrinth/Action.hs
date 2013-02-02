module Labyrinth.Action where

import Control.Monad.State

import Labyrinth.Map
import Labyrinth.Move

import Peeker

performMove :: Move -> State Labyrinth MoveResult
performMove (Move []) = return $ MoveRes []
performMove (Move (act:acts)) = do
    ar <- performAction act
    (MoveRes ars) <- performMove $ Move acts
    return $ MoveRes $ ar:ars

performAction :: Action -> State Labyrinth ActionResult
performAction (Go dir) = do
    l <- get
    let pi = getP currentPlayer l
    let p = getP (player pi) l
    let pp = getP position p
    let w = getP (wall pp dir) l
    if w == NoWall then do
                             let npp = advance pp dir
                             let nl = updP (player pi ~> position) l npp
                             put nl
                             let nc = getP (cell npp ~> ctype) l
                             return $ GoR $ WentOnto nc
    else return $ GoR HitWall

