{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestLabyrinth.ShowLabyrinth (htf_thisModulesTests) where

import Labyrinth

import Control.Lens

import Data.List

import Test.Framework
import TestLabyrinth.Common

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

interesting_expected = intercalate "\n" $ [ "+==+==+==+--+==+==+"
                                          , "X.  A  v  .  2  . X"
                                          , "+  +  +  +  +  +  +"
                                          , "X.  .  v  .  3  . X"
                                          , "+  +  +  +  +  +  +"
                                          , "X.  O  < |1 |.  . X"
                                          , "+  +  +  +  +  +  +"
                                          , " .  .  .  .  .  . X"
                                          , "+  +  +  +  +  +  +"
                                          , "X.  .  H  .  .  . X"
                                          , "+==+==+==+==+  +==+"
                                          , ""
                                          , "0: Player (1, 1), 3B, 3G"
                                          , "1: Player (3, 3), 0B, 3G, wounded"
                                          , "Current player: 0"
                                          , "(5, 2): 1B, 1G"
                                          , "(1, 3): fake treasure"
                                          , "(5, 3): true treasure"
                                          , "(4, 4): 2B"
                                          , "(5, 4): 2G"
                                          ]

interesting_wounded = applyState interesting_labyrinth $ do
    (player 1 . phealth) .= Wounded
    (player 1 . pbullets) .= 0
    (cell (Pos 4 4) . cbullets) .= 2
    (cell (Pos 5 4) . cgrenades) .= 2
    (cell (Pos 5 2) . cbullets) .= 1
    (cell (Pos 5 2) . cgrenades) .= 1

test_show_empty = assertShowEquals empty_expected empty_labyrinth

test_show_wounded = assertShowEquals interesting_expected interesting_wounded
