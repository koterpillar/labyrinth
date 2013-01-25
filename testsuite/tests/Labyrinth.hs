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

player_one = Player { position = Pos 0 0
                    , bullets = 3
                    , grenades = 3
                    , treasure = Nothing
                    }

empty_labyrinth = Labyrinth { cells = replicate 5 $ replicate 5 $ (Cell Land)
                            , wallsH = replicate 5 $ replicate 6 $ NoWall
                            , wallsV = replicate 6 $ replicate 5 $ NoWall
                            , players = []
                            , currentPlayer = 0
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
    let l = empty_labyrinth
    let m = Move $ [Go D]
    assertEqual "movement only move"
        (MoveRes [GoR $ WentOnto Land], l) $
        runState (performMove m) l
