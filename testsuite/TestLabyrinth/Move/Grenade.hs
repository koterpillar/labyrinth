{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestLabyrinth.Move.Grenade where

import Labyrinth

import Control.Lens
import Control.Monad.State

import Test.Framework
import TestLabyrinth.Common
import TestLabyrinth.Move.Common

test_wall = do
    assertMoveUpdates'
        walled_labyrinth
        (Move [Grenade R])
        (MoveRes [GrenadeR GrenadeOK])
        $ do
            (wall (Pos 0 0) R) .= NoWall
            (player 0 . pgrenades) .= 2
            currentTurn .= 1

test_no_wall = do
    assertMoveUpdates'
        empty_labyrinth
        (Move [Grenade R])
        (MoveRes [GrenadeR GrenadeOK])
        $ do
            (player 0 . pgrenades) .= 2
            currentTurn .= 1

test_hard_wall = do
    assertMoveUpdates'
        walled_labyrinth
        (Move [Grenade L])
        (MoveRes [GrenadeR GrenadeOK])
        $ do
            (player 0 . pgrenades) .= 2
            currentTurn .= 1

test_no_grenades = do
    assertMoveUpdates'
        (applyState walled_labyrinth $ (player 0 . pgrenades) .= 0)
        (Move [Grenade R])
        (MoveRes [GrenadeR NoGrenades])
        $ do
            currentTurn .= 1

test_found_grenades = do
    let found_grenades = applyState walled_labyrinth $ do
        (player 0 . position) .= Pos 1 1
        (player 0 . pgrenades) .= 2
        (cell (Pos 1 1) . cgrenades) .= 2
    assertMoveUpdates'
        found_grenades
        (Move [Grenade L, Grenade R, Grenade U, Grenade D])
        (MoveRes $ replicate 4 $ GrenadeR GrenadeOK)
        $ do
            currentTurn .= 1
            (cell (Pos 1 1) . cgrenades) .= 0
            (player 0 . pgrenades) .= 0
            forM_ [L, R, U, D] $ \d -> (wall (Pos 1 1) d) .= NoWall

test_from_armory = do
    let from_armory = applyState walled_labyrinth $ do
        (player 0 . position) .= Pos 1 1
        (cell (Pos 1 1) . ctype) .= Armory
    assertMoveUpdates'
        from_armory
        (Move [Grenade L, Grenade R, Grenade U, Grenade D])
        (MoveRes $ replicate 4 $ GrenadeR GrenadeOK)
        $ do
            currentTurn .= 1
            forM_ [L, R, U, D] $ \d -> (wall (Pos 1 1) d) .= NoWall
