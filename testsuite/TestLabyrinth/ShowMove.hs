{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# Language TemplateHaskell #-}

module TestLabyrinth.ShowMove (htf_thisModulesTests) where

import Labyrinth

import Data.DeriveTH

import Test.Framework
import TestLabyrinth.Common

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

derive makeArbitrary ''Direction
derive makeArbitrary ''MoveDirection
derive makeArbitrary ''Action
derive makeArbitrary ''Move
derive makeArbitrary ''CellTypeResult
derive makeArbitrary ''TreasureResult
derive makeArbitrary ''GoResult
derive makeArbitrary ''ShootResult
derive makeArbitrary ''GrenadeResult
derive makeArbitrary ''ActionResult
derive makeArbitrary ''MoveResult

prop_show_parse_move :: Move -> Bool
prop_show_parse_move m = parsed == m
    where
        parseResult = parseMove $ show m
        parsed = case parseResult of
            Right x -> x
            Left y -> error y

prop_show_move_result :: MoveResult -> Bool
prop_show_move_result = (0 <) . length . show
