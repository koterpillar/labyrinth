import Labyrinth

import Control.Monad.State

import Data.List

import Peeker

import Test.HUnit hiding (State)

main = runTestTT tests

tests = TestList [ test_advance
                 , test_show_labyrinth
                 , test_show_move
                 , test_show_move_result
                 , test_move
                 , test_move_to_armory
                 , test_move_to_pit
                 , test_move_to_river
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

interesting_labyrinth = applyState empty_labyrinth $ do
    updS (player 0 ~> position) (Pos 1 1)
    updS (player 1 ~> position) (Pos 3 3)
    updS (cell (Pos 1 0) ~> ctype) Armory
    updS (cell (Pos 2 0) ~> ctype) $ River D
    updS (cell (Pos 4 0) ~> ctype) $ Pit 1
    updS (cell (Pos 2 1) ~> ctype) $ River D
    updS (cell (Pos 4 1) ~> ctype) $ Pit 2
    updS (cell (Pos 1 2) ~> ctype) RiverDelta
    updS (cell (Pos 2 2) ~> ctype) $ River L
    updS (cell (Pos 3 2) ~> ctype) $ Pit 0

interesting_expected = intercalate "\n" $ [ "+==+==+==+==+==+==+"
                                          , "X.  A  v  .  2  . X"
                                          , "+  +  +  +  +  +  +"
                                          , "X.  .  v  .  3  . X"
                                          , "+  +  +  +  +  +  +"
                                          , "X.  O  <  1  .  . X"
                                          , "+  +  +  +  +  +  +"
                                          , "X.  .  .  .  .  . X"
                                          , "+  +  +  +  +  +  +"
                                          , "X.  .  .  .  .  . X"
                                          , "+==+==+==+==+==+==+"
                                          , ""
                                          , "0: Player (1, 1), 3B, 3G"
                                          , "1: Player (3, 3), 3B, 3G"
                                          , "Current player: 0"
                                          ]

test_show_labyrinth = TestCase $ do
    assertEqual "empty labyrinth"
        empty_expected $
        show empty_labyrinth
    assertEqual "empty labyrinth"
        interesting_expected $
        show interesting_labyrinth

assertShowEquals :: (Show a) => String -> a -> Assertion
assertShowEquals message move = assertEqual message message $ show move

test_show_move = TestCase $ do
    assertShowEquals "skip" $
        Move []
    assertShowEquals "go left" $
        Move [goTowards L]
    assertShowEquals "go right" $
        Move [goTowards R]
    assertShowEquals "go down" $
        Move [goTowards D]
    assertShowEquals "go up" $
        Move [goTowards U]
    assertShowEquals "shoot left, go up, grenade left" $
        Move [Shoot L, goTowards U, Grenade L]

test_show_move_result = TestCase $ do
    assertShowEquals "ok" $
        MoveRes []
    assertShowEquals "hit a wall" $
        MoveRes [GoR $ HitWall]
    assertShowEquals "went onto land" $
        MoveRes [GoR $ Went LandR 0 0 0 Nothing]
    assertShowEquals "went onto land, found a bullet" $
        MoveRes [GoR $ Went LandR 1 0 0 Nothing]
    assertShowEquals "went onto land, found 2 bullets" $
        MoveRes [GoR $ Went LandR 2 0 0 Nothing]
    assertShowEquals "went onto land, found 2 bullets, 3 grenades and a treasure" $
        MoveRes [GoR $ Went LandR 2 3 1 Nothing]
    assertShowEquals "went onto river, was transported to river, found 2 grenades" $
        MoveRes [GoR $ Went RiverR 0 2 0 (Just RiverR)]

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
        (MoveRes [GoR $ Went LandR 0 0 0 Nothing])
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
        (MoveRes [GoR $ Went ArmoryR 0 0 0 Nothing])
        $ do
            updS (player 0 ~> position) (Pos 1 0)
            updS (player 0 ~> pbullets) 3
            updS (player 0 ~> pgrenades) 3
            updS currentPlayer 1

test_move_to_pit = TestCase $ do
    let lab = applyState interesting_labyrinth $ do
        updS currentPlayer 1
        updS (cell (Pos 3 2) ~> cbullets) 1
        updS (cell (Pos 4 0) ~> cgrenades) 1
        updS (player 1 ~> pgrenades) 0
        updS (player 1 ~> pbullets) 0
    assertMoveUpdates "went into pit"
        lab
        (Move [goTowards U])
        (MoveRes [GoR $ Went PitR 0 1 0 (Just PitR)])
        $ do
            updS (player 1 ~> position) (Pos 4 0)
            updS (player 1 ~> pgrenades) 1
            updS currentPlayer 0
            updS (cell (Pos 4 0) ~> cgrenades) 0

test_move_to_river = TestCase $ do
    let lab = applyState interesting_labyrinth $ do
        updS (cell (Pos 2 1) ~> cbullets) 1
        updS (cell (Pos 2 2) ~> cgrenades) 1
        updS (player 0 ~> pgrenades) 0
        updS (player 0 ~> pbullets) 0
    assertMoveUpdates "went into river"
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
    assertMoveUpdates "went into river, carried into delta"
        lab2
        (Move [goTowards U])
        (MoveRes [GoR $ Went RiverR 0 1 0 (Just RiverDeltaR)])
        $ do
            updS (player 0 ~> position) (Pos 1 2)
            updS (player 0 ~> pgrenades) 1
            updS currentPlayer 1
            updS (cell (Pos 1 2) ~> cgrenades) 0

test_found_ammo = TestCase $ do
    let empty_ammo = applyState empty_labyrinth $ do
        updS (player 0 ~> pbullets) 0
        updS (player 0 ~> pgrenades) 0
        updS (cell (Pos 0 1) ~> cbullets) 2
        updS (cell (Pos 0 1) ~> cgrenades) 1
    assertMoveUpdates "found bullets and grenades"
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
    assertMoveUpdates "found too much"
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

test_found_treasure = TestCase $ do
    let empty_treasure = applyState empty_labyrinth $ do
        updS (cell (Pos 0 1) ~> ctreasures) [FakeTreasure]
    assertMoveUpdates "found a treasure"
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
    assertMoveUpdates "found a treasure while having one already"
        empty_treasure_2
        (Move [goTowards D])
        (MoveRes [GoR $ Went LandR 0 0 1 Nothing])
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
        (MoveRes [GrenadeR GrenadeOK, GoR $ Went LandR 0 0 0 Nothing])
        $ do
            updS (player 0 ~> position) (Pos 1 0)
            updS (player 0 ~> pgrenades) 2
            updS (wall (Pos 0 0) R) NoWall
            updS currentPlayer 1
