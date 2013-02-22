{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestLabyrinth.Move.ChoosePosition where

import Labyrinth

import Control.Monad.State

import Peeker

import Test.Framework
import TestLabyrinth.Common
import TestLabyrinth.Move.Common

l0 = applyState empty_labyrinth $ do
    updS positionsChosen False

test_all_ok = do
    let (r1, l1) = runState (performMove 0 $ ChoosePosition $ Pos 2 2) l0
    let l1_expected = applyState l1 $ do
        updS (player 0 ~> position) $ Pos 2 2
        updS currentPlayer 1
    assertEqual r1 $ ChoosePositionR ChosenOK
    assertEqual l1 l1_expected
    let (r2, l2) = runState (performMove 1 $ ChoosePosition $ Pos 3 3) l1
    let l2_expected = applyState l1 $ do
        updS (player 1 ~> position) $ Pos 3 3
        updS currentPlayer 0
        updS positionsChosen True
    assertEqual r2 $ ChoosePositionR AllChosenOK
    assertEqual l2 l2_expected

test_outside = do
    assertMoveUpdates'
        l0
        (ChoosePosition $ Pos (-1) (-1))
        InvalidMove
        $ do
            return ()

test_not_chosen = do
    assertMoveUpdates'
        l0
        (Move [])
        InvalidMove
        $ do
            return ()
    assertMoveUpdates'
        l0
        (Move [goTowards R])
        InvalidMove
        $ do
            return ()
    assertMoveUpdates'
        l0
        (Move [Grenade R])
        InvalidMove
        $ do
            return ()

test_already_chosen = do
    assertMoveUpdates'
        empty_labyrinth
        (ChoosePosition $ Pos 1 1)
        InvalidMove
        $ do
            return ()
