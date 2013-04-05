{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestLabyrinth.Move.ChoosePosition where

import Labyrinth

import Control.Lens
import Control.Monad.State

import Test.Framework
import TestLabyrinth.Common
import TestLabyrinth.Move.Common

l0 = applyState interesting_labyrinth $ do
    positionsChosen .= False

test_all_ok = do
    let (r1, l1) = runState (performMove 0 $ ChoosePosition $ Pos 2 2) l0
    let l1_expected = applyState l1 $ do
        (player 0 . position) .= Pos 2 2
        currentTurn .= 1
    assertEqual (MoveRes [ChoosePositionR ChosenOK]) r1
    assertEqual l1_expected l1
    let (r2, l2) = runState (performMove 1 $ ChoosePosition $ Pos 3 3) l1
    let l2_expected = applyState l1 $ do
        (player 0 . position) .= Pos 1 2
        (player 1 . position) .= Pos 3 3
        currentTurn .= 0
        positionsChosen .= True
    let player_results = [ StartR 0 RiverR $ CellEvents 0 0 0 $ Just RiverDeltaR
                         , StartR 1 LandR noEvents
                         ]
    assertEqual (MoveRes [GameStarted player_results]) r2
    assertEqual l2_expected l2

test_outside = do
    assertMoveUpdates'
        l0
        (ChoosePosition $ Pos (-1) (-1))
        (MoveRes [InvalidMove])
        $ do
            return ()

test_not_chosen = do
    assertMoveUpdates'
        l0
        (Move [])
        (MoveRes [InvalidMove])
        $ do
            return ()
    assertMoveUpdates'
        l0
        (Move [goTowards R])
        (MoveRes [InvalidMove])
        $ do
            return ()
    assertMoveUpdates'
        l0
        (Move [Grenade R])
        (MoveRes [InvalidMove])
        $ do
            return ()

test_already_chosen = do
    assertMoveUpdates'
        empty_labyrinth
        (ChoosePosition $ Pos 1 1)
        (MoveRes [InvalidMove])
        $ do
            return ()
