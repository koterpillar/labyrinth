import Labyrinth

import Control.Monad.State

import Data.List

import Peeker

import Test.HUnit hiding (State)

main = runTestTT tests

tests = TestList [ test_advance
                 , test_show
                 , test_move
                 , test_move_to_armory
                 , test_found_ammo
                 , test_found_treasure
                 , test_grenade
                 , test_combined
                 ]

test_advance = TestCase $ do
    assertEqual "going down"
        (Pos 0 1) $
        advance (Pos 0 0) D

w = 6
h = 5

player_one = initialPlayer $ Pos 0 0
player_two = initialPlayer $ Pos 2 2

empty_labyrinth = emptyLabyrinth w h [Pos 0 0, Pos 2 2]

applyState = flip execState

walled_labyrinth = applyState empty_labyrinth $ do
    forM_ [0..w-1] $
        \x -> forM_ [0..h-2] $
            \y -> do
                updS (wall (Pos x y) D) Wall
    forM_ [0..w-2] $
        \x -> forM_ [0..h-1] $
            \y -> do
                updS (wall (Pos x y) R) Wall

empty_expected = intercalate "\n" $ [ "+==+==+==+==+==+==+"
                                    , "X.  .  .  .  .  . X"
                                    , "+  +  +  +  +  +  +"
                                    , "X.  .  .  .  .  . X"
                                    , "+  +  +  +  +  +  +"
                                    , "X.  .  .  .  .  . X"
                                    , "+  +  +  +  +  +  +"
                                    , "X.  .  .  .  .  . X"
                                    , "+  +  +  +  +  +  +"
                                    , "X.  .  .  .  .  . X"
                                    , "+==+==+==+==+==+==+"
                                    , ""
                                    , "0: Player (0, 0), 3B, 3G"
                                    , "1: Player (2, 2), 3B, 3G"
                                    , "Current player: 0"
                                    ]

test_show = TestCase $ do
    assertEqual "empty labyrinth"
        empty_expected $
        show $ empty_labyrinth

interesting_labyrinth = applyState empty_labyrinth $ do
    updS (player 0 ~> position) (Pos 1 1)
    updS (player 1 ~> position) (Pos 3 3)
    updS (cell (Pos 1 0) ~> ctype) Armory

assertMoveUpdates :: String -> Labyrinth -> Move -> MoveResult -> State Labyrinth () -> Assertion
assertMoveUpdates message initialLab move result labUpdate = do
    let updatedLab = execState labUpdate initialLab
    assertEqual message
        (result, updatedLab) $
        runState (performMove move) initialLab

test_move = TestCase $ do
    assertMoveUpdates "movement only - hit wall"
        walled_labyrinth
        (Move [goTowards D])
        (MoveRes [GoR HitWall])
        $ do
            updS currentPlayer 1
    assertMoveUpdates "movement only - went onto land"
        empty_labyrinth
        (Move [goTowards D])
        (MoveRes [GoR $ Went Land 0 0 0 Nothing])
        $ do
            updS (player 0 ~> position) (Pos 0 1)
            updS currentPlayer 1

test_move_to_armory = TestCase $ do
    let armory_missing_ammo = applyState interesting_labyrinth $ do
        updS (player 0 ~> pbullets) 2
        updS (player 0 ~> pgrenades) 1
    assertMoveUpdates "move to armoury"
        armory_missing_ammo
        (Move [goTowards U])
        (MoveRes [GoR $ Went Armory 0 0 0 Nothing])
        $ do
            updS (player 0 ~> position) (Pos 1 0)
            updS (player 0 ~> pbullets) 3
            updS (player 0 ~> pgrenades) 3
            updS currentPlayer 1

test_found_ammo = TestCase $ do
    let empty_ammo = applyState empty_labyrinth $ do
        updS (player 0 ~> pbullets) 0
        updS (player 0 ~> pgrenades) 0
        updS (cell (Pos 0 1) ~> cbullets) 2
        updS (cell (Pos 0 1) ~> cgrenades) 1
    assertMoveUpdates "found bullets and grenades"
        empty_ammo
        (Move [goTowards D])
        (MoveRes [GoR $ Went Land 2 1 0 Nothing])
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
    assertMoveUpdates "found too much"
        empty_ammo_2
        (Move [goTowards D])
        (MoveRes [GoR $ Went Land 4 5 0 Nothing])
        $ do
            updS (player 0 ~> position) (Pos 0 1)
            updS (player 0 ~> pbullets) 3
            updS (player 0 ~> pgrenades) 3
            updS (cell (Pos 0 1) ~> cbullets) 3
            updS (cell (Pos 0 1) ~> cgrenades) 5
            updS currentPlayer 1

test_found_treasure = TestCase $ do
    let empty_treasure = applyState empty_labyrinth $ do
        updS (cell (Pos 0 1) ~> ctreasures) [FakeTreasure]
    assertMoveUpdates "found a treasure"
        empty_treasure
        (Move [goTowards D])
        (MoveRes [GoR $ Went Land 0 0 1 Nothing])
        $ do
            updS (player 0 ~> position) (Pos 0 1)
            updS (player 0 ~> ptreasure) (Just FakeTreasure)
            updS (cell (Pos 0 1) ~> ctreasures) []
            updS currentPlayer 1
    let empty_treasure_2 = applyState empty_labyrinth $ do
        updS (player 0 ~> ptreasure) (Just FakeTreasure)
        updS (cell (Pos 0 1) ~> ctreasures) [TrueTreasure]
    assertMoveUpdates "found a treasure while having one already"
        empty_treasure_2
        (Move [goTowards D])
        (MoveRes [GoR $ Went Land 0 0 1 Nothing])
        $ do
            updS (player 0 ~> position) (Pos 0 1)
            updS currentPlayer 1

test_grenade = TestCase $ do
    assertMoveUpdates "grenade, wall"
        walled_labyrinth
        (Move [Grenade R])
        (MoveRes [GrenadeR GrenadeOK])
        $ do
            updS (wall (Pos 0 0) R) NoWall
            updS (player 0 ~> pgrenades) 2
            updS currentPlayer 1
    assertMoveUpdates "grenade, no wall"
        empty_labyrinth
        (Move [Grenade R])
        (MoveRes [GrenadeR GrenadeOK])
        $ do
            updS (player 0 ~> pgrenades) 2
            updS currentPlayer 1
    assertMoveUpdates "grenade, hard wall"
        walled_labyrinth
        (Move [Grenade L])
        (MoveRes [GrenadeR GrenadeOK])
        $ do
            updS (player 0 ~> pgrenades) 2
            updS currentPlayer 1
    assertMoveUpdates "no grenades"
        (applyState walled_labyrinth $ updS (player 0 ~> pgrenades) 0)
        (Move [Grenade R])
        (MoveRes [GrenadeR NoGrenades])
        $ do
            updS currentPlayer 1

test_combined = TestCase $ do
    assertMoveUpdates "move then grenade"
        walled_labyrinth
        (Move [Grenade R, goTowards R])
        (MoveRes [GrenadeR GrenadeOK, GoR $ Went Land 0 0 0 Nothing])
        $ do
            updS (player 0 ~> position) (Pos 1 0)
            updS (player 0 ~> pgrenades) 2
            updS (wall (Pos 0 0) R) NoWall
            updS currentPlayer 1
