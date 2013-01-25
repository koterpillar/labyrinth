module Labyrinth.Action where

import Control.Monad.State

import Labyrinth.Map
import Labyrinth.Move

performMove :: Move -> State Labyrinth MoveResult
performMove (Move []) = return $ MoveRes []
performMove (Move (act:acts)) = do
    ar <- performAction act
    (MoveRes ars) <- performMove $ Move acts
    return $ MoveRes $ ar:ars

performAction :: Action -> State Labyrinth ActionResult
performAction (Go dir) = do
    l <- get
    let p = currentPlayer l
    let pp = position $ player l p
    let wall = wallAt l pp dir
    act wall
    where act NoWall   = undefined
          act Wall     = return $ GoR HitWall
          act HardWall = return $ GoR HitWall

