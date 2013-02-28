{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestLabyrinth.Move.Common where

import Labyrinth

import Control.Monad.State

import TestLabyrinth.Common

import Peeker

import Test.Framework
import qualified Test.HUnit as HU

assertMoveUpdates :: Labyrinth -> PlayerId -> Move -> MoveResult -> State Labyrinth () -> HU.Assertion
assertMoveUpdates initialLab pi move result labUpdate = do
    let updatedLab = execState labUpdate initialLab
    assertEqual
        (result, updatedLab) $
        runState (performMove pi move) initialLab

assertMoveUpdates' :: Labyrinth -> Move -> MoveResult -> State Labyrinth () -> HU.Assertion
assertMoveUpdates' initialLab = assertMoveUpdates initialLab pi
    where pi = getP currentTurn initialLab
