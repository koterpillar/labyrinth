{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestLabyrinth.Move.Surrender where

import Labyrinth

import Control.Lens
import Control.Monad.State

import Test.Framework
import TestLabyrinth.Common
import TestLabyrinth.Move.Common

test_surrender = do
    let l0 = empty_labyrinth
    let (r1, l1) = runState (performMove 0 $ Move [Surrender]) l0
    assertEqual (MoveRes [Surrendered]) r1
    let l1_expected = applyState l0 $ do
        (player 0 . phealth) .= Dead
        (player 0 . pbullets) .= 0
        (player 0 . pgrenades) .= 0
        (cell (Pos 0 0). cbullets) .= 3
        (cell (Pos 0 0). cgrenades) .= 3
        currentTurn .= 1
    assertEqual l1_expected l1
    let (r2, l2) = runState (performMove 1 $ Move [Surrender]) l1
    assertEqual (MoveRes [Surrendered, Draw]) r2
    let l2_expected = applyState l1 $ do
        (player 1 . phealth) .= Dead
        (player 1 . pbullets) .= 0
        (player 1 . pgrenades) .= 0
        (cell (Pos 2 2). cbullets) .= 3
        (cell (Pos 2 2). cgrenades) .= 3
        gameEnded .= True
    assertEqual l2_expected l2
