{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestLabyrinth.Show (htf_thisModulesTests) where

import Labyrinth

import Test.Framework
import qualified Test.HUnit as HU

assertShowEquals :: (Show a) => String -> a -> HU.Assertion
assertShowEquals message move = assertEqual message $ show move

test_show_move = do
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

test_show_move_result = do
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
