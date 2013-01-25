import Labyrinth

import Control.Monad.State

import Data.List

import Test.HUnit

main = runTestTT tests

tests = TestList [ test_show
                 , test_move
                 ]

empty_labyrinth = Labyrinth { cells = replicate 5 $ replicate 5 $ (Cell Land)
                            , wallsH = replicate 5 $ replicate 6 $ NoWall
                            , wallsV = replicate 6 $ replicate 5 $ NoWall
                            , players = []
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
