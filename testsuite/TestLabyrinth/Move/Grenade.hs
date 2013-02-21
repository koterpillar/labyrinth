{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestLabyrinth.Move.Grenade where

import Labyrinth

import Control.Monad.State

import Peeker

import Test.Framework
import TestLabyrinth.Common
import TestLabyrinth.Move.Common

test_wall = do
    assertMoveUpdates'
        walled_labyrinth
        (Move [Grenade R])
        (MoveRes [GrenadeR GrenadeOK])
        $ do
            updS (wall (Pos 0 0) R) NoWall
            updS (player 0 ~> pgrenades) 2
            updS currentPlayer 1

test_no_wall = do
    assertMoveUpdates'
        empty_labyrinth
        (Move [Grenade R])
        (MoveRes [GrenadeR GrenadeOK])
        $ do
            updS (player 0 ~> pgrenades) 2
            updS currentPlayer 1

test_hard_wall = do
    assertMoveUpdates'
        walled_labyrinth
        (Move [Grenade L])
        (MoveRes [GrenadeR GrenadeOK])
        $ do
            updS (player 0 ~> pgrenades) 2
            updS currentPlayer 1

test_no_grenades = do
    assertMoveUpdates'
        (applyState walled_labyrinth $ updS (player 0 ~> pgrenades) 0)
        (Move [Grenade R])
        (MoveRes [GrenadeR NoGrenades])
        $ do
            updS currentPlayer 1

test_found_grenades = do
    let found_grenades = applyState walled_labyrinth $ do
        updS (player 0 ~> position) $ Pos 1 1
        updS (player 0 ~> pgrenades) 2
        updS (cell (Pos 1 1) ~> cgrenades) 2
    assertMoveUpdates'
        found_grenades
        (Move [Grenade L, Grenade R, Grenade U, Grenade D])
        (MoveRes $ replicate 4 $ GrenadeR GrenadeOK)
        $ do
            updS currentPlayer 1
            updS (cell (Pos 1 1) ~> cgrenades) 0
            updS (player 0 ~> pgrenades) 0
            forM_ [L, R, U, D] $ \d -> updS (wall (Pos 1 1) d) NoWall

test_from_armory = do
    let from_armory = applyState walled_labyrinth $ do
        updS (player 0 ~> position) $ Pos 1 1
        updS (cell (Pos 1 1) ~> ctype) $ Armory
    assertMoveUpdates'
        from_armory
        (Move [Grenade L, Grenade R, Grenade U, Grenade D])
        (MoveRes $ replicate 4 $ GrenadeR GrenadeOK)
        $ do
            updS currentPlayer 1
            forM_ [L, R, U, D] $ \d -> updS (wall (Pos 1 1) d) NoWall
