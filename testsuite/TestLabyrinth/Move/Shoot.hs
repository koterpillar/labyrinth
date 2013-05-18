{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestLabyrinth.Move.Shoot where

import Labyrinth

import Control.Lens
import Control.Monad.State

import Test.Framework
import TestLabyrinth.Common
import TestLabyrinth.Move.Common

shootPos = Pos 0 2
target = Pos 2 2
duel = applyState empty_labyrinth $ do
    (player 0 . position) .= shootPos
passTurnBullet = do
    currentTurn .= 1
    (player 0 . pbullets) -= 1
hit' pi h = do
    (player pi . phealth) .= h
    (player pi . pbullets) .= 0
    (cell target . cbullets) .= 3
hit = hit' 1
fell' pi = (player pi . pjustShot) .= True
fell = fell' 1

test_healthy = do
    assertMoveUpdates'
        duel
        (Move [Shoot R])
        (MoveRes [ShootR Scream, WoundedAlert 1 Wounded])
        $ do
            passTurnBullet
            hit Wounded
            fell

test_wounded = do
    let duel_wounded = applyState duel $ do
        (player 1 . phealth) .= Wounded
        (player 1 . pbullets) .= 0
    assertMoveUpdates'
        duel_wounded
        (Move [Shoot R])
        (MoveRes [ShootR Scream, WoundedAlert 1 Dead])
        $ do
            (player 0 . pbullets) .= 2
            hit Dead
            (player 1 . pgrenades) .= 0
            (cell target . cbullets) .= 0
            (cell target . cgrenades) .= 3

test_wounded_alert = do
    let duel3 = applyState (emptyLabyrinth 5 5 3) $ do
        (player 0 . position) .= Pos 0 2
        (player 1 . position) .= Pos 4 4
        (player 2 . position) .= Pos 2 2
        positionsChosen .= True
    let move0 = (performMove 0 $ Move [Shoot R])
    let (r1, l1) = runState move0 duel3
    assertEqual (MoveRes [ShootR Scream]) r1
    let l1_expected = applyState duel3 $ do
        passTurnBullet
        hit' 2 Wounded
        fell' 2
    assertEqual l1_expected l1
    let (r2, l2) = runState (performMove 1 $ Move [goTowards U]) l1
    assertEqual
        (MoveRes [GoR $ Went LandR noEvents, WoundedAlert 2 Wounded])
        r2
    let l2_expected = applyState l1 $ do
        currentTurn .= 2
        (player 1 . position) .= Pos 4 3
    assertEqual l2_expected l2
    let (r3, l3) = runState (performMove 2 $ Move []) l2
    assertEqual (MoveRes []) r3
    let l3_expected = applyState l2 $ do
        currentTurn .= 0
        (player 2 . pjustShot) .= False
    assertEqual l3_expected l3
    let (r4, l4) = runState move0 l3
    assertEqual (MoveRes [ShootR Scream]) r4
    let l4_expected = applyState l3 $ do
        passTurnBullet
        hit' 2 Dead
        fell' 2
        (player 2 . pgrenades) .= 0
        (cell (Pos 2 2) . cgrenades) .= 3
    assertEqual l4_expected l4
    let (r5, l5) = runState (performMove 1 $ Move [goTowards D]) l4
    assertEqual
        (MoveRes [GoR $ Went LandR noEvents, WoundedAlert 2 Dead])
        r5
    let l5_expected = applyState l4 $ do
        currentTurn .= 0
        (player 1 . position) .= Pos 4 4
        (player 2 . pjustShot) .= False
    assertEqual l5_expected l5

test_treasure = do
    let duel_treasure = applyState duel $ do
        (player 1 . ptreasure) .= Just FakeTreasure
    assertMoveUpdates'
        duel_treasure
        (Move [Shoot R])
        (MoveRes [ShootR Scream, WoundedAlert 1 Wounded])
        $ do
            passTurnBullet
            hit Wounded
            (player 1 . ptreasure) .= Nothing
            (cell target . ctreasures) .= [FakeTreasure]
            fell

test_through_wall = do
    let duel_wall = applyState duel $ do
        (wall shootPos R) .= Wall
    assertMoveUpdates'
        duel_wall
        (Move [Shoot R])
        (MoveRes [ShootR ShootOK])
        $ do
            passTurnBullet

test_from_hospital = do
    let duel_from_hospital = applyState duel $ do
        (cell shootPos . ctype) .= Hospital
    assertMoveUpdates'
        duel_from_hospital
        (Move [Shoot R])
        (MoveRes [ShootR Forbidden])
        $ do
            currentTurn .= 1

test_from_armory = do
    let duel_from_armory = applyState duel $ do
        (cell shootPos . ctype) .= Armory
    assertMoveUpdates'
        duel_from_armory
        (Move [Shoot R])
        (MoveRes [ShootR Forbidden])
        $ do
            currentTurn .= 1

test_through_hospital = do
    let duel_through_hospital = applyState duel $ do
        (cell (Pos 1 2) . ctype) .= Hospital
    assertMoveUpdates'
        duel_through_hospital
        (Move [Shoot R])
        (MoveRes [ShootR ShootOK])
        $ passTurnBullet

test_through_armory = do
    let duel_through_armory = applyState duel $ do
        (cell (Pos 1 2) . ctype) .= Armory
    assertMoveUpdates'
        duel_through_armory
        (Move [Shoot R])
        (MoveRes [ShootR ShootOK])
        $ do
            passTurnBullet

test_into_hospital = do
    let duel_into_hospital = applyState duel $ do
        (cell target . ctype) .= Hospital
    assertMoveUpdates'
        duel_into_hospital
        (Move [Shoot R])
        (MoveRes [ShootR ShootOK])
        $ do
            passTurnBullet

test_into_armory = do
    let duel_into_armory = applyState duel $ do
        (cell target . ctype) .= Armory
    assertMoveUpdates'
        duel_into_armory
        (Move [Shoot R])
        (MoveRes [ShootR Scream, WoundedAlert 1 Wounded])
        $ do
            passTurnBullet
            hit Wounded
            fell

test_outside = do
    let target' = Pos 6 2
    let duel_outside = applyState duel $ do
        (wall (Pos 5 2) R) .= NoWall
        (player 1 . position) .= target'
    assertMoveUpdates'
        duel_outside
        (Move [Shoot R])
        (MoveRes [ShootR Scream, WoundedAlert 1 Wounded])
        $ do
            passTurnBullet
            (player 1 . phealth) .= Wounded
            (player 1 . pbullets) .= 0
            fell

test_double_shot = do
    assertMoveUpdates'
        duel
        (Move [Shoot R, Shoot R])
        (MoveRes [ShootR Scream, ShootR ShootOK, WoundedAlert 1 Wounded])
        $ do
            passTurnBullet
            (player 0 . pbullets) .= 1
            hit Wounded
            fell

test_found_bullets = do
    let duel_bullets = applyState duel $ do
        (cell shootPos . cbullets) .= 2
    assertMoveUpdates'
        duel_bullets
        (Move $ replicate 5 $ Shoot U)
        (MoveRes $ replicate 5 $ ShootR ShootOK)
        $ do
            passTurnBullet
            (cell shootPos . cbullets) .= 0
            (player 0 . pbullets) .= 0

test_reorder_cell = do
    let duel = applyState (emptyLabyrinth 5 6 2) $ do
        (player 0 . position) .= Pos 0 0
        (player 1 . position) .= Pos 2 0
        (player 1 . pbullets) .= 0
        positionsChosen .= True
        (cell (Pos 2 1) . ctype) .= Hospital
    let shoot = Move [Shoot R]
    let skip = Move []
    let (r1, l1) = runState (performMove 0 shoot) duel
    assertEqual (MoveRes [ShootR Scream, WoundedAlert 1 Wounded]) r1
    let l1_expected = applyState duel $ do
        currentTurn .= 1
        (player 0 . pbullets) .= 2
        (player 1 . phealth) .= Wounded
        (player 1 . pjustShot) .= True
    assertEqual l1_expected l1
    let (r2, l2) = runState (performMove 1 $ ReorderCell $ Pos 2 1) l1
    assertEqual (MoveRes [ReorderCellR $ ReorderOK HospitalR noEvents]) r2
    let l2_expected = applyState l2 $ do
        (player 1 . pjustShot) .= False
        (player 1 . position) .= Pos 2 1
    assertEqual l2_expected l2
    let (r3, l3) = runState (performMove 1 $ Move [goTowards U]) l2
    assertEqual (MoveRes [GoR $ Went LandR noEvents]) r3
    let l3_expected = applyState l2 $ do
        currentTurn .= 0
        (player 1 . position) .= Pos 2 0
    assertEqual l3_expected l3
    let (r4, l4) = runState (performMove 0 shoot) duel
    assertEqual (MoveRes [ShootR Scream, WoundedAlert 1 Wounded]) r4
