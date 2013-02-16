{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestLabyrinth.ShowLabyrinth (htf_thisModulesTests) where

import Labyrinth

import Data.List

import Peeker

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
                                          , "X.  O  <  1  .  . X"
                                          , "+  +  +  +  +  +  +"
                                          , " .  .  .  .  .  . X"
                                          , "+  +  +  +  +  +  +"
                                          , "X.  .  H  .  .  . X"
                                          , "+==+==+==+==+  +==+"
                                          , ""
                                          , "0: Player (1, 1), 3B, 3G"
                                          , "1: Player (3, 3), 0B, 3G, wounded"
                                          , "Current player: 0"
                                          , "(1, 3): fake treasure"
                                          , "(5, 3): true treasure"
                                          ]

interesting_wounded = applyState interesting_labyrinth $ do
    updS (player 1 ~> phealth) Wounded
    updS (player 1 ~> pbullets) 0

test_show_labyrinth = do
    assertShowEquals empty_expected empty_labyrinth
    assertShowEquals interesting_expected interesting_wounded
