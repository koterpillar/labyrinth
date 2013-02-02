import Labyrinth

import Control.Monad.State

import Data.List

import Peeker

import Test.HUnit

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

test_move = TestCase $ do
    let m = Move $ [Go D]
    let walled_lab_updated = (flip execState) walled_labyrinth $ do
        updS currentPlayer 1
    assertEqual "movement only - hit wall"
        (MoveRes [GoR $ HitWall], walled_lab_updated) $
        runState (performMove m) walled_labyrinth
    let empty_lab_updated = (flip execState) empty_labyrinth $ do
        updS (player 0 ~> position) (Pos 0 1)
        updS currentPlayer 1
    assertEqual "movement only move"
        (MoveRes [GoR $ WentOnto Land], empty_lab_updated) $
        runState (performMove m) empty_labyrinth
