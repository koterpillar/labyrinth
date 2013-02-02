import Labyrinth

import Control.Monad.State

import Data.List

import Peeker

import Test.HUnit hiding (State)

main = runTestTT tests

tests = TestList [ test_advance
                 , test_show
                 , test_move
                 ]

test_advance = TestCase $ do
    assertEqual "going down"
        (Pos 0 1) $
        advance (Pos 0 0) D

player_one = Player { position_ = Pos 0 0
                    , bullets_  = 3
                    , grenades_ = 3
                    , treasure_ = Nothing
                    }

player_two = Player { position_ = Pos 2 2
                    , bullets_  = 3
                    , grenades_ = 3
                    , treasure_ = Nothing
                    }

empty_labyrinth = Labyrinth { cells_         = replicate 5 $ replicate 5 $ (Cell Land)
                            , wallsH_        = replicate 5 $ replicate 6 $ NoWall
                            , wallsV_        = replicate 6 $ replicate 5 $ NoWall
                            , players_       = [ player_one
                                               , player_two
                                               ]
                            , currentPlayer_ = 0
                            }

walled_labyrinth = Labyrinth { cells_         = replicate 5 $ replicate 5 $ (Cell Land)
                             , wallsH_        = replicate 5 $ replicate 6 $ Wall
                             , wallsV_        = replicate 6 $ replicate 5 $ Wall
                             , players_       = [ player_one
                                                , player_two
                                                ]
                             , currentPlayer_ = 0
                             }

empty_expected = intercalate "\n" $ [ "+  +  +  +  +  +"
                                    , " .  .  .  .  .  "
                                    , "+  +  +  +  +  +"
                                    , " .  .  .  .  .  "
                                    , "+  +  +  +  +  +"
                                    , " .  .  .  .  .  "
                                    , "+  +  +  +  +  +"
                                    , " .  .  .  .  .  "
                                    , "+  +  +  +  +  +"
                                    , " .  .  .  .  .  "
                                    , "+  +  +  +  +  +"
                                    , ""
                                    , "0: Player (0, 0)"
                                    , "1: Player (2, 2)"
                                    , "Current player: 0"
                                    ]

test_show = TestCase $ do
    assertEqual "empty labyrinth"
        empty_expected $
        show empty_labyrinth

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
