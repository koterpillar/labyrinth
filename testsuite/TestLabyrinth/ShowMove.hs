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
derive makeArbitrary ''Position
derive makeArbitrary ''Move
derive makeArbitrary ''CellTypeResult
derive makeArbitrary ''TreasureResult
derive makeArbitrary ''GoResult
derive makeArbitrary ''ShootResult
derive makeArbitrary ''GrenadeResult
derive makeArbitrary ''ActionResult
derive makeArbitrary ''ChoosePositionResult
derive makeArbitrary ''MoveResult

isSecret :: Move -> Bool
isSecret (ChoosePosition _) = True
isSecret _                  = False

prop_show_move :: Move -> Bool
prop_show_move = (0 <) . length . show

prop_show_parse_move :: Move -> Property
prop_show_parse_move m = not (isSecret m) ==> parsed == m
    where
        parseResult = parseMove $ show m
        parsed = case parseResult of
            Right x -> x
            Left y -> error y

prop_show_choose_position :: Move -> Move -> Property
prop_show_choose_position m1 m2 =
    isChoose m1 && isChoose m2 ==> show m1 == show m2
    where isChoose (ChoosePosition _) = True
          isChoose _                  = False

test_parse_choose_position = do
    assertEqual
        (Right $ ChoosePosition $ Pos 2 3)
        $ parseMove "choose 2 3"

prop_show_move_result :: MoveResult -> Bool
prop_show_move_result = (0 <) . length . show
