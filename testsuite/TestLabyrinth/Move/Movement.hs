{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestLabyrinth.Move.Movement where

import Labyrinth

import Control.Lens
import Control.Monad.State

import Test.Framework
import TestLabyrinth.Common
import TestLabyrinth.Move.Common

test_land = do
    assertMoveUpdates'
        walled_labyrinth
        (Move [goTowards D])
        (MoveRes [GoR $ HitWall noEvents])
        $ do
            currentTurn .= 1
    assertMoveUpdates'
        empty_labyrinth
        (Move [goTowards D])
        (MoveRes [GoR $ Went LandR noEvents])
        $ do
            (player 0 . position) .= Pos 0 1
            currentTurn .= 1

test_to_armory = do
    let armory_missing_ammo = applyState interesting_labyrinth $ do
        (player 0 . pbullets) .= 2
        (player 0 . pgrenades) .= 1
    assertMoveUpdates'
        armory_missing_ammo
        (Move [goTowards U])
        (MoveRes [GoR $ Went ArmoryR noEvents])
        $ do
            (player 0 . position) .= Pos 1 0
            (player 0 . pbullets) .= 3
            (player 0 . pgrenades) .= 3
            currentTurn .= 1

test_to_hospital = do
    let lab = applyState interesting_labyrinth $ do
        (player 0 . phealth) .= Wounded
        (player 0 . pbullets) .= 0
        (player 0 . position) .= Pos 1 4
    assertMoveUpdates'
        lab
        (Move [goTowards R])
        (MoveRes [GoR $ Went HospitalR noEvents])
        $ do
            (player 0 . position) .= Pos 2 4
            (player 0 . phealth) .= Healthy
            currentTurn .= 1

test_to_pit = do
    let lab = applyState interesting_labyrinth $ do
        currentTurn .= 1
        (cell (Pos 3 2) . cbullets) .= 1
        (cell (Pos 4 0) . cgrenades) .= 1
        (player 1 . pgrenades) .= 0
        (player 1 . pbullets) .= 0
    assertMoveUpdates'
        lab
        (Move [goTowards U])
        (MoveRes [GoR $ Went PitR $ CellEvents 0 1 0 (Just PitR)])
        $ do
            (player 1 . position) .= Pos 4 0
            (player 1 . pgrenades) .= 1
            currentTurn .= 0
            (cell (Pos 4 0) . cgrenades) .= 0
    let lab2 = applyState interesting_labyrinth $ do
        (player 0 . position) .= Pos 3 2
    assertMoveUpdates'
        lab2
        (Move [goTowards R])
        (MoveRes [GoR $ HitWall noEvents])
        $ do
            currentTurn .= 1
    assertMoveUpdates'
        lab2
        (Move [Go Next])
        (MoveRes [GoR $ Went PitR $ CellEvents 0 0 0 (Just PitR)])
        $ do
            currentTurn .= 1
            (player 0 . position) .= Pos 4 0

test_to_river = do
    let lab = applyState interesting_labyrinth $ do
        (cell (Pos 2 1) . cbullets) .= 1
        (cell (Pos 2 2) . cgrenades) .= 1
        (player 0 . pgrenades) .= 0
        (player 0 . pbullets) .= 0
    assertMoveUpdates'
        lab
        (Move [goTowards R])
        (MoveRes [GoR $ Went RiverR $ CellEvents 0 1 0 (Just RiverR)])
        $ do
            (player 0 . position) .= Pos 2 2
            (player 0 . pgrenades) .= 1
            currentTurn .= 1
            (cell (Pos 2 2) . cgrenades) .= 0
    let lab2 = applyState interesting_labyrinth $ do
        (cell (Pos 2 2) . cbullets) .= 1
        (cell (Pos 1 2) . cgrenades) .= 1
        (player 0 . position) .= Pos 2 3
        (player 0 . pgrenades) .= 0
        (player 0 . pbullets) .= 0
    assertMoveUpdates'
        lab2
        (Move [goTowards U])
        (MoveRes [GoR $ Went RiverR $ CellEvents 0 1 0 (Just RiverDeltaR)])
        $ do
            (player 0 . position) .= Pos 1 2
            (player 0 . pgrenades) .= 1
            currentTurn .= 1
            (cell (Pos 1 2) . cgrenades) .= 0
    let lab3 = applyState interesting_labyrinth $ do
        (player 0 . position) .= Pos 2 2
    assertMoveUpdates'
        lab3
        (Move [goTowards R])
        (MoveRes [GoR $ HitWall $ CellEvents 0 0 0 (Just RiverDeltaR)])
        $ do
            (player 0 . position) .= Pos 1 2
            currentTurn .= 1
    let lab4 = applyState interesting_labyrinth $ do
        (player 0 . position) .= Pos 2 1
    assertMoveUpdates'
        lab4
        (Move [Go Next])
        (MoveRes [GoR $ Went RiverR $ CellEvents 0 0 0 (Just RiverR)])
        $ do
            currentTurn .= 1
            (player 0 . position) .= Pos 2 2

test_found_ammo = do
    let empty_ammo = applyState empty_labyrinth $ do
        (player 0 . pbullets) .= 0
        (player 0 . pgrenades) .= 0
        (cell (Pos 0 1) . cbullets) .= 2
        (cell (Pos 0 1) . cgrenades) .= 1
    assertMoveUpdates'
        empty_ammo
        (Move [goTowards D])
        (MoveRes [GoR $ Went LandR $ CellEvents 2 1 0 Nothing])
        $ do
            (player 0 . position) .= Pos 0 1
            (player 0 . pbullets) .= 2
            (player 0 . pgrenades) .= 1
            (cell (Pos 0 1) . cbullets) .= 0
            (cell (Pos 0 1) . cgrenades) .= 0
            currentTurn .= 1
    let empty_ammo_2 = applyState empty_labyrinth $ do
        (player 0 . pbullets) .= 2
        (player 0 . pgrenades) .= 3
        (cell (Pos 0 1) . cbullets) .= 4
        (cell (Pos 0 1) . cgrenades) .= 5
    assertMoveUpdates'
        empty_ammo_2
        (Move [goTowards D])
        (MoveRes [GoR $ Went LandR $ CellEvents 4 5 0 Nothing])
        $ do
            (player 0 . position) .= Pos 0 1
            (player 0 . pbullets) .= 3
            (player 0 . pgrenades) .= 3
            (cell (Pos 0 1) . cbullets) .= 3
            (cell (Pos 0 1) . cgrenades) .= 5
            currentTurn .= 1

test_found_treasure = do
    let empty_treasure = applyState empty_labyrinth $ do
        (cell (Pos 0 1) . ctreasures) .= [FakeTreasure]
    assertMoveUpdates'
        empty_treasure
        (Move [goTowards D])
        (MoveRes [GoR $ Went LandR $ CellEvents 0 0 1 Nothing])
        $ do
            (player 0 . position) .= Pos 0 1
            (player 0 . ptreasure) .= Just FakeTreasure
            (cell (Pos 0 1) . ctreasures) .= []
            currentTurn .= 1
    let empty_treasure_2 = applyState empty_labyrinth $ do
        (player 0 . ptreasure) .= Just FakeTreasure
        (cell (Pos 0 1) . ctreasures) .= [TrueTreasure]
    assertMoveUpdates'
        empty_treasure_2
        (Move [goTowards D])
        (MoveRes [GoR $ Went LandR $ CellEvents 0 0 1 Nothing])
        $ do
            (player 0 . position) .= Pos 0 1
            currentTurn .= 1

test_outside = do
    let lab_no_treasure = applyState interesting_labyrinth $ do
        (player 0 . position) .= Pos 0 3
    assertMoveUpdates'
        lab_no_treasure
        (Move [goTowards L, Grenade U])
        (MoveRes [GoR $ WentOutside Nothing, GrenadeR GrenadeOK])
        $ do
            currentTurn .= 1
            (player 0 . position) .= Pos (-1) 3
            (player 0 . pgrenades) .= 2
    let lab_fake_treasure = applyState lab_no_treasure $ do
        (player 0 . ptreasure) .= Just FakeTreasure
    assertMoveUpdates'
        lab_fake_treasure
        (Move [goTowards L, Grenade U])
        (MoveRes [GoR $ WentOutside $ Just TurnedToAshesR, GrenadeR GrenadeOK])
        $ do
            currentTurn .= 1
            (player 0 . position) .= Pos (-1) 3
            (player 0 . ptreasure) .= Nothing
            (player 0 . pgrenades) .= 2
    let lab_true_treasure = applyState lab_no_treasure $ do
        (player 0 . ptreasure) .= Just TrueTreasure
    assertMoveUpdates'
        lab_true_treasure
        (Move [goTowards L, Grenade U])
        (MoveRes [GoR $ WentOutside $ Just TrueTreasureR])
        $ do
            currentTurn .= 1
            (player 0 . position) .= Pos (-1) 3
            (player 0 . ptreasure) .= Nothing
            gameEnded .= True
    let bottom = applyState interesting_labyrinth $ do
        (player 0 . position) .= Pos 4 4
    assertMoveUpdates'
        bottom
        (Move [goTowards D])
        (MoveRes [GoR $ WentOutside Nothing])
        $ do
            currentTurn .= 1
            (player 0 . position) .= Pos 4 5
    let die_outside = applyState interesting_labyrinth $ do
        (player 0 . position) .= Pos 4 5
    assertMoveUpdates'
        die_outside
        (Move [goTowards D, Grenade U])
        (MoveRes [GoR LostOutside])
        $ do
            currentTurn .= 1
            (player 0 . phealth) .= Dead
            (player 0 . pbullets) .= 0
            (player 0 . pgrenades) .= 0
    let draw_outside = applyState die_outside $ do
        (player 1 . phealth) .= Dead
        (player 1 . pbullets) .= 0
        (player 1 . pgrenades) .= 0
    assertMoveUpdates'
        draw_outside
        (Move [goTowards D, Grenade U])
        (MoveRes [GoR LostOutside, Draw])
        $ do
            (player 0 . phealth) .= Dead
            (player 0 . pbullets) .= 0
            (player 0 . pgrenades) .= 0
            gameEnded .= True
