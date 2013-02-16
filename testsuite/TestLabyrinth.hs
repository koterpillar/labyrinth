{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestLabyrinth where

import Labyrinth

import Control.Monad.State

import Peeker

import qualified Test.HUnit as HU
import Test.Framework
import TestLabyrinth.Common

test_advance = do
    assertEqual
        (Pos 0 1) $
        advance (Pos 0 0) D

assertMoveUpdates :: Labyrinth -> PlayerId -> Move -> MoveResult -> State Labyrinth () -> HU.Assertion
assertMoveUpdates initialLab pi move result labUpdate = do
    let updatedLab = execState labUpdate initialLab
    assertEqual
        (result, updatedLab) $
        runState (performMove pi move) initialLab

assertMoveUpdates' :: Labyrinth -> Move -> MoveResult -> State Labyrinth () -> HU.Assertion
assertMoveUpdates' initialLab = assertMoveUpdates initialLab pi
    where pi = getP currentPlayer initialLab

test_wrong_turn = do
    assertMoveUpdates
        empty_labyrinth
        1
        (Move [goTowards D])
        WrongTurn
        $ return ()

test_move = do
    assertMoveUpdates'
        walled_labyrinth
        (Move [goTowards D])
        (MoveRes [GoR HitWall])
        $ do
            updS currentPlayer 1
    assertMoveUpdates'
        empty_labyrinth
        (Move [goTowards D])
        (MoveRes [GoR $ Went LandR 0 0 0 Nothing])
        $ do
            updS (player 0 ~> position) (Pos 0 1)
            updS currentPlayer 1

test_move_to_armory = do
    let armory_missing_ammo = applyState interesting_labyrinth $ do
        updS (player 0 ~> pbullets) 2
        updS (player 0 ~> pgrenades) 1
    assertMoveUpdates'
        armory_missing_ammo
        (Move [goTowards U])
        (MoveRes [GoR $ Went ArmoryR 0 0 0 Nothing])
        $ do
            updS (player 0 ~> position) (Pos 1 0)
            updS (player 0 ~> pbullets) 3
            updS (player 0 ~> pgrenades) 3
            updS currentPlayer 1

test_move_to_hospital = do
    let lab = applyState interesting_labyrinth $ do
        updS (player 0 ~> phealth) Wounded
        updS (player 0 ~> pbullets) 0
        updS (player 0 ~> position) $ Pos 1 4
    assertMoveUpdates'
        lab
        (Move [goTowards R])
        (MoveRes [GoR $ Went HospitalR 0 0 0 Nothing])
        $ do
            updS (player 0 ~> position) $ Pos 2 4
            updS (player 0 ~> phealth) Healthy
            updS currentPlayer 1

test_move_to_pit = do
    let lab = applyState interesting_labyrinth $ do
        updS currentPlayer 1
        updS (cell (Pos 3 2) ~> cbullets) 1
        updS (cell (Pos 4 0) ~> cgrenades) 1
        updS (player 1 ~> pgrenades) 0
        updS (player 1 ~> pbullets) 0
    assertMoveUpdates'
        lab
        (Move [goTowards U])
        (MoveRes [GoR $ Went PitR 0 1 0 (Just PitR)])
        $ do
            updS (player 1 ~> position) (Pos 4 0)
            updS (player 1 ~> pgrenades) 1
            updS currentPlayer 0
            updS (cell (Pos 4 0) ~> cgrenades) 0

test_move_to_river = do
    let lab = applyState interesting_labyrinth $ do
        updS (cell (Pos 2 1) ~> cbullets) 1
        updS (cell (Pos 2 2) ~> cgrenades) 1
        updS (player 0 ~> pgrenades) 0
        updS (player 0 ~> pbullets) 0
    assertMoveUpdates'
        lab
        (Move [goTowards R])
        (MoveRes [GoR $ Went RiverR 0 1 0 (Just RiverR)])
        $ do
            updS (player 0 ~> position) (Pos 2 2)
            updS (player 0 ~> pgrenades) 1
            updS currentPlayer 1
            updS (cell (Pos 2 2) ~> cgrenades) 0
    let lab2 = applyState interesting_labyrinth $ do
        updS (cell (Pos 2 2) ~> cbullets) 1
        updS (cell (Pos 1 2) ~> cgrenades) 1
        updS (player 0 ~> position) (Pos 2 3)
        updS (player 0 ~> pgrenades) 0
        updS (player 0 ~> pbullets) 0
    assertMoveUpdates'
        lab2
        (Move [goTowards U])
        (MoveRes [GoR $ Went RiverR 0 1 0 (Just RiverDeltaR)])
        $ do
            updS (player 0 ~> position) (Pos 1 2)
            updS (player 0 ~> pgrenades) 1
            updS currentPlayer 1
            updS (cell (Pos 1 2) ~> cgrenades) 0

test_found_ammo = do
    let empty_ammo = applyState empty_labyrinth $ do
        updS (player 0 ~> pbullets) 0
        updS (player 0 ~> pgrenades) 0
        updS (cell (Pos 0 1) ~> cbullets) 2
        updS (cell (Pos 0 1) ~> cgrenades) 1
    assertMoveUpdates'
        empty_ammo
        (Move [goTowards D])
        (MoveRes [GoR $ Went LandR 2 1 0 Nothing])
        $ do
            updS (player 0 ~> position) (Pos 0 1)
            updS (player 0 ~> pbullets) 2
            updS (player 0 ~> pgrenades) 1
            updS (cell (Pos 0 1) ~> cbullets) 0
            updS (cell (Pos 0 1) ~> cgrenades) 0
            updS currentPlayer 1
    let empty_ammo_2 = applyState empty_labyrinth $ do
        updS (player 0 ~> pbullets) 2
        updS (player 0 ~> pgrenades) 3
        updS (cell (Pos 0 1) ~> cbullets) 4
        updS (cell (Pos 0 1) ~> cgrenades) 5
    assertMoveUpdates'
        empty_ammo_2
        (Move [goTowards D])
        (MoveRes [GoR $ Went LandR 4 5 0 Nothing])
        $ do
            updS (player 0 ~> position) (Pos 0 1)
            updS (player 0 ~> pbullets) 3
            updS (player 0 ~> pgrenades) 3
            updS (cell (Pos 0 1) ~> cbullets) 3
            updS (cell (Pos 0 1) ~> cgrenades) 5
            updS currentPlayer 1

test_found_treasure = do
    let empty_treasure = applyState empty_labyrinth $ do
        updS (cell (Pos 0 1) ~> ctreasures) [FakeTreasure]
    assertMoveUpdates'
        empty_treasure
        (Move [goTowards D])
        (MoveRes [GoR $ Went LandR 0 0 1 Nothing])
        $ do
            updS (player 0 ~> position) (Pos 0 1)
            updS (player 0 ~> ptreasure) (Just FakeTreasure)
            updS (cell (Pos 0 1) ~> ctreasures) []
            updS currentPlayer 1
    let empty_treasure_2 = applyState empty_labyrinth $ do
        updS (player 0 ~> ptreasure) (Just FakeTreasure)
        updS (cell (Pos 0 1) ~> ctreasures) [TrueTreasure]
    assertMoveUpdates'
        empty_treasure_2
        (Move [goTowards D])
        (MoveRes [GoR $ Went LandR 0 0 1 Nothing])
        $ do
            updS (player 0 ~> position) (Pos 0 1)
            updS currentPlayer 1

test_grenade = do
    assertMoveUpdates'
        walled_labyrinth
        (Move [Grenade R])
        (MoveRes [GrenadeR GrenadeOK])
        $ do
            updS (wall (Pos 0 0) R) NoWall
            updS (player 0 ~> pgrenades) 2
            updS currentPlayer 1
    assertMoveUpdates'
        empty_labyrinth
        (Move [Grenade R])
        (MoveRes [GrenadeR GrenadeOK])
        $ do
            updS (player 0 ~> pgrenades) 2
            updS currentPlayer 1
    assertMoveUpdates'
        walled_labyrinth
        (Move [Grenade L])
        (MoveRes [GrenadeR GrenadeOK])
        $ do
            updS (player 0 ~> pgrenades) 2
            updS currentPlayer 1
    assertMoveUpdates'
        (applyState walled_labyrinth $ updS (player 0 ~> pgrenades) 0)
        (Move [Grenade R])
        (MoveRes [GrenadeR NoGrenades])
        $ do
            updS currentPlayer 1

test_shoot = do
    let duel = applyState empty_labyrinth $ do
        updS (player 0 ~> position) $ Pos 0 2
    assertMoveUpdates'
        duel
        (Move [Shoot R])
        (MoveRes [ShootR Scream])
        $ do
            updS currentPlayer 1
            updS (player 0 ~> pbullets) 2
            updS (player 1 ~> phealth) Wounded
            updS (player 1 ~> pbullets) 0
            updS (cell (Pos 2 2) ~> cbullets) 3

test_outside = do
    let lab_no_treasure = applyState interesting_labyrinth $ do
        updS (player 0 ~> position) (Pos 0 3)
    assertMoveUpdates'
        lab_no_treasure
        (Move [goTowards L, Grenade U])
        (MoveRes [GoR $ WentOutside Nothing, GrenadeR GrenadeOK])
        $ do
            updS currentPlayer 1
            updS (player 0 ~> position) (Pos (-1) 3)
            updS (player 0 ~> pgrenades) 2
    let lab_fake_treasure = applyState lab_no_treasure $ do
        updS (player 0 ~> ptreasure) $ Just FakeTreasure
    assertMoveUpdates'
        lab_fake_treasure
        (Move [goTowards L, Grenade U])
        (MoveRes [GoR $ WentOutside $ Just TurnedToAshesR, GrenadeR GrenadeOK])
        $ do
            updS currentPlayer 1
            updS (player 0 ~> position) (Pos (-1) 3)
            updS (player 0 ~> ptreasure) Nothing
            updS (player 0 ~> pgrenades) 2
    let lab_true_treasure = applyState lab_no_treasure $ do
        updS (player 0 ~> ptreasure) $ Just TrueTreasure
    assertMoveUpdates'
        lab_true_treasure
        (Move [goTowards L, Grenade U])
        (MoveRes [GoR $ WentOutside $ Just TrueTreasureR])
        $ do
            updS currentPlayer 1
            updS (player 0 ~> position) (Pos (-1) 3)
            updS (player 0 ~> ptreasure) Nothing

test_combined = do
    assertMoveUpdates'
        walled_labyrinth
        (Move [Grenade R, goTowards R])
        (MoveRes [GrenadeR GrenadeOK, GoR $ Went LandR 0 0 0 Nothing])
        $ do
            updS (player 0 ~> position) (Pos 1 0)
            updS (player 0 ~> pgrenades) 2
            updS (wall (Pos 0 0) R) NoWall
            updS currentPlayer 1

test_invalid = do
    assertMoveUpdates'
        walled_labyrinth
        (Move [goTowards R, goTowards R])
        InvalidMove
        $ return ()

