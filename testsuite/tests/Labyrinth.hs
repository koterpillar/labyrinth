import Labyrinth

import Control.Monad.State

import Data.List

import Peeker

import Test.HUnit hiding (State)

main = runTestTT tests

tests = TestList [ test_advance
                 , test_show
                 , test_move
                 , test_grenade
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

walled_labyrinth = (flip execState) empty_labyrinth $ do
    forM_ [0..w-2] $
        \x -> forM_ [0..h-2] $
            \y -> do
                updS (wall (Pos x y) R) Wall
                updS (wall (Pos x y) D) Wall

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
                                    , "0: Player (0, 0)"
                                    , "1: Player (2, 2)"
                                    , "Current player: 0"
                                    ]

test_show = TestCase $ do
    assertEqual "empty labyrinth"
        empty_expected $
        show $ empty_labyrinth

assertMoveUpdates :: String -> Labyrinth -> Move -> MoveResult -> State Labyrinth () -> Assertion
assertMoveUpdates message initialLab move result labUpdate = do
    let updatedLab = execState labUpdate initialLab
    assertEqual message
        (result, updatedLab) $
        runState (performMove move) initialLab

test_move = TestCase $ do
    assertMoveUpdates "movement only - hit wall"
        walled_labyrinth
        (Move [Go D])
        (MoveRes [GoR HitWall])
        $ do
            updS currentPlayer 1
    assertMoveUpdates "movement only - went onto land"
        empty_labyrinth
        (Move [Go D])
        (MoveRes [GoR $ WentOnto Land])
        $ do
            updS (player 0 ~> position) (Pos 0 1)
            updS currentPlayer 1

test_grenade = TestCase $ do
    assertMoveUpdates "grenade wall"
        walled_labyrinth
        (Move [Grenade R])
        (MoveRes [GrenadeR GrenadeOK])
        $ do
            updS (wall (Pos 0 0) R) NoWall
            updS currentPlayer 1
    assertMoveUpdates "grenade no wall"
        empty_labyrinth
        (Move [Grenade R])
        (MoveRes [GrenadeR GrenadeOK])
        $ do
            updS currentPlayer 1
    assertMoveUpdates "grenade hard wall"
        walled_labyrinth
        (Move [Grenade L])
        (MoveRes [GrenadeR GrenadeOK])
        $ do
            updS currentPlayer 1
