{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestLabyrinth.Move.Shoot where

import Labyrinth

import Control.Monad.State

import Peeker

import Test.Framework
import TestLabyrinth.Common
import TestLabyrinth.Move.Common

shootPos = Pos 0 2
target = Pos 2 2
duel = applyState empty_labyrinth $ do
    updS (player 0 ~> position) shootPos
passTurnBullet = do
    updS currentPlayer 1
    updS (player 0 ~> pbullets) 2
hit h = do
    updS (player 1 ~> phealth) h
    updS (player 1 ~> pbullets) 0
    updS (cell target ~> cbullets) 3
fell = updS (player 1 ~> pfell) True

test_healthy = do
    assertMoveUpdates'
        duel
        (Move [Shoot R])
        (MoveRes [ShootR Scream])
        $ do
            passTurnBullet
            hit Wounded
            fell

test_wounded = do
    let duel_wounded = applyState duel $ do
        updS (player 1 ~> phealth) Wounded
        updS (player 1 ~> pbullets) 0
    assertMoveUpdates'
        duel_wounded
        (Move [Shoot R])
        (MoveRes [ShootR Scream])
        $ do
            updS (player 0 ~> pbullets) 2
            hit Dead
            updS (player 1 ~> pgrenades) 0
            updS (cell target ~> cbullets) 0
            updS (cell target ~> cgrenades) 3

test_through_wall = do
    let duel_wall = applyState duel $ do
        updS (wall shootPos R) Wall
    assertMoveUpdates'
        duel_wall
        (Move [Shoot R])
        (MoveRes [ShootR ShootOK])
        $ do
            passTurnBullet

test_from_hospital = do
    let duel_from_hospital = applyState duel $ do
        updS (cell shootPos ~> ctype) Hospital
    assertMoveUpdates'
        duel_from_hospital
        (Move [Shoot R])
        (MoveRes [ShootR Forbidden])
        $ do
            updS currentPlayer 1

test_from_armory = do
    let duel_from_armory = applyState duel $ do
        updS (cell shootPos ~> ctype) Armory
    assertMoveUpdates'
        duel_from_armory
        (Move [Shoot R])
        (MoveRes [ShootR Forbidden])
        $ do
            updS currentPlayer 1

test_through_hospital = do
    let duel_through_hospital = applyState duel $ do
        updS (cell (Pos 1 2) ~> ctype) Hospital
    assertMoveUpdates'
        duel_through_hospital
        (Move [Shoot R])
        (MoveRes [ShootR ShootOK])
        $ passTurnBullet

test_through_armory = do
    let duel_through_armory = applyState duel $ do
        updS (cell (Pos 1 2) ~> ctype) Armory
    assertMoveUpdates'
        duel_through_armory
        (Move [Shoot R])
        (MoveRes [ShootR ShootOK])
        $ do
            passTurnBullet

test_into_hospital = do
    let duel_into_hospital = applyState duel $ do
        updS (cell target ~> ctype) Hospital
    assertMoveUpdates'
        duel_into_hospital
        (Move [Shoot R])
        (MoveRes [ShootR ShootOK])
        $ do
            passTurnBullet

test_into_armory = do
    let duel_into_armory = applyState duel $ do
        updS (cell target ~> ctype) Armory
    assertMoveUpdates'
        duel_into_armory
        (Move [Shoot R])
        (MoveRes [ShootR Scream])
        $ do
            passTurnBullet
            hit Wounded
            fell

test_outside = do
    let target' = Pos 6 2
    let duel_outside = applyState duel $ do
        updS (wall (Pos 5 2) R) NoWall
        updS (player 1 ~> position) target'
    assertMoveUpdates'
        duel_outside
        (Move [Shoot R])
        (MoveRes [ShootR Scream])
        $ do
            passTurnBullet
            updS (player 1 ~> phealth) Wounded
            updS (player 1 ~> pbullets) 0
            fell

test_double_shot = do
    assertMoveUpdates'
        duel
        (Move [Shoot R, Shoot R])
        (MoveRes [ShootR Scream, ShootR ShootOK])
        $ do
            passTurnBullet
            updS (player 0 ~> pbullets) 1
            hit Wounded
            fell

test_found_bullets = do
    let duel_bullets = applyState duel $ do
        updS (cell shootPos ~> cbullets) 2
    assertMoveUpdates'
        duel_bullets
        (Move $ replicate 5 $ Shoot U)
        (MoveRes $ replicate 5 $ ShootR ShootOK)
        $ do
            passTurnBullet
            updS (cell shootPos ~> cbullets) 0
            updS (player 0 ~> pbullets) 0

test_reorder_cell = do
    let duel = applyState (emptyLabyrinth 5 6 2) $ do
        updS (player 0 ~> position) $ Pos 0 0
        updS (player 1 ~> position) $ Pos 2 0
        updS (player 1 ~> pbullets) 0
        updS positionsChosen True
        updS (cell (Pos 2 1) ~> ctype) Hospital
    let shoot = Move [Shoot R]
    let skip = Move []
    let (r1, l1) = runState (performMove 0 shoot) duel
    assertEqual (MoveRes [ShootR Scream]) r1
    let l1_expected = applyState duel $ do
        updS currentPlayer 1
        updS (player 0 ~> pbullets) 2
        updS (player 1 ~> phealth) Wounded
        updS (player 1 ~> pfell) True
    assertEqual l1_expected l1
    let (r2, l2) = runState (performMove 1 $ ReorderCell $ Pos 2 1) l1
    assertEqual (ReorderCellR $ ReorderOK HospitalR noEvents) r2
    let l2_expected = applyState l2 $ do
        updS (player 1 ~> pfell) False
        updS (player 1 ~> position) $ Pos 2 1
    assertEqual l2_expected l2
    let (r3, l3) = runState (performMove 1 $ Move [goTowards U]) l2
    assertEqual (MoveRes [GoR $ Went LandR noEvents]) r3
    let l3_expected = applyState l2 $ do
        updS currentPlayer 0
        updS (player 1 ~> position) $ Pos 2 0
    assertEqual l3_expected l3
    let (r4, l4) = runState (performMove 0 shoot) duel
    assertEqual (MoveRes [ShootR Scream]) r4
