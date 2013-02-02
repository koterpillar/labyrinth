import Labyrinth

import Control.Monad.State

import Data.List

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

empty_labyrinth = Labyrinth { cells_         = replicate 5 $ replicate 5 $ (Cell Land)
                            , wallsH_        = replicate 5 $ replicate 6 $ NoWall
                            , wallsV_        = replicate 6 $ replicate 5 $ NoWall
                            , players_       = [ player_one
                                               ]
                            , currentPlayer_ = 0
                            }

walled_labyrinth = Labyrinth { cells_         = replicate 5 $ replicate 5 $ (Cell Land)
                             , wallsH_        = replicate 5 $ replicate 6 $ Wall
                             , wallsV_        = replicate 6 $ replicate 5 $ Wall
                             , players_       = [ player_one
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
                                    ]

test_show = TestCase $ do
    assertEqual "empty labyrinth"
        empty_expected $
        show empty_labyrinth

test_move = TestCase $ do
    let m = Move $ [Go D]
    assertEqual "movement only - hit wall"
        (MoveRes [GoR $ HitWall], walled_labyrinth) $
        runState (performMove m) walled_labyrinth
    assertEqual "movement only move"
        (MoveRes [GoR $ WentOnto Land], empty_labyrinth) $
        runState (performMove m) empty_labyrinth
