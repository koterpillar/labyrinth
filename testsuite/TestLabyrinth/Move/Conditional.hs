{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestLabyrinth.Move.Conditional where

import Labyrinth

import Control.Lens
import Control.Monad.State

import Test.Framework
import TestLabyrinth.Common
import TestLabyrinth.Move.Common

test_condition_true = do
    assertMoveUpdates'
        (applyState empty_labyrinth (wall (Pos 0 0) R .= Wall))
        (Move [goTowards R, Conditional "hit a wall" [Shoot D] [Grenade D]])
        (MoveRes [GoR $ HitWall noEvents, ShootR ShootOK])
        $ do
            currentTurn .= 1
            (player 0 . pbullets) .= 2

test_condition_false = do
    assertMoveUpdates'
        empty_labyrinth
        (Move [goTowards R, Conditional "hit a wall" [Shoot D] [Grenade D]])
        (MoveRes [GoR $ HitWall noEvents, GrenadeR GrenadeOK])
        $ do
            currentTurn .= 1
            (player 0 . pgrenades) .= 2
