{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# Language TemplateHaskell #-}

module TestLabyrinth.ShowMove (htf_thisModulesTests) where

import Labyrinth

import Control.Applicative
import Control.Monad

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
    assertShowEquals "go next" $
        Move [Go Next]
    assertShowEquals "shoot left, go up, grenade left" $
        Move [Shoot L, goTowards U, Grenade L]
    assertShowEquals "surrender" $
        Move [Surrender]
    assertShowEquals "go left, if hit a wall { shoot up } else { shoot down }" $
        Move [goTowards L, Conditional "hit a wall" [Shoot U] [Shoot D]]
    assertShowEquals "go left, if hit a wall { shoot up }" $
        Move [goTowards L, Conditional "hit a wall" [Shoot U] []]
    assertShowEquals "query bullets, treasure, health, grenades" $
        Query [BulletCount, TreasureCarried, PlayerHealth, GrenadeCount]
    assertShowEquals "say hello" $
        Say "hello"

test_parse_move = do
    assertEqual
        (Right $ ChoosePosition $ Pos 2 3)
        $ parseMove "choose 2 3"
    assertEqual
        (Right $ Move [goTowards R])
        $ parseMove "go right"
    assertEqual
        (Right $ Move [goTowards R])
        $ parseMove "move right"
    assertEqual
        (Right $ Say "hello")
        $ parseMove "say hello"

test_show_move_result = do
    assertShowEquals "ok" $
        MoveRes []
    assertShowEquals "hit a wall" $
        MoveRes [GoR $ HitWall noEvents]
    assertShowEquals "went onto land" $
        MoveRes [GoR $ Went LandR noEvents]
    assertShowEquals "went onto land, found a bullet" $
        MoveRes [GoR $ Went LandR $ CellEvents 1 0 0 Nothing]
    assertShowEquals "went onto land, found 2 bullets" $
        MoveRes [GoR $ Went LandR $ CellEvents 2 0 0 Nothing]
    assertShowEquals "went onto land, found 2 bullets, 3 grenades and a treasure" $
        MoveRes [GoR $ Went LandR $ CellEvents 2 3 1 Nothing]
    assertShowEquals "went onto river, was transported to river, found 2 grenades" $
        MoveRes [GoR $ Went RiverR $ CellEvents 0 2 0 (Just RiverR)]
    assertShowEquals "3 bullets, 1 grenade, no treasure, wounded" $
        MoveRes $ map QueryR $ [ BulletCountR 3
                               , GrenadeCountR 1
                               , TreasureCarriedR False
                               , HealthR Wounded
                               ]
    assertShowEquals "game started; player 0 started at land, found a treasure; player 1 started at hospital" $
        MoveRes [ GameStarted [ StartR 0 LandR $ CellEvents 0 0 1 Nothing
                              , StartR 1 HospitalR $ noEvents
                              ]
                ]

derive makeArbitrary ''Direction
derive makeArbitrary ''MoveDirection

simpleAction = oneof [ Go <$> arbitrary
                     , Shoot <$> arbitrary
                     , Grenade <$> arbitrary
                     , return Surrender
                     ]

instance Arbitrary Action where
    arbitrary = oneof [ simpleAction
                      , Conditional
                            <$> return "condition"
                            <*> listOf simpleAction
                            <*> listOf simpleAction
                      ]

derive makeArbitrary ''QueryType
derive makeArbitrary ''Position

instance Arbitrary Move where
    arbitrary = oneof [ liftM Move arbitrary
                      , liftM ChoosePosition arbitrary
                      , liftM ReorderCell arbitrary
                      , liftM Query $ listOf1 arbitrary
                      , liftM Say arbitrary
                      ]

derive makeArbitrary ''CellTypeResult
derive makeArbitrary ''CellEvents
derive makeArbitrary ''TreasureResult
derive makeArbitrary ''GoResult
derive makeArbitrary ''ShootResult
derive makeArbitrary ''GrenadeResult
derive makeArbitrary ''ActionResult
derive makeArbitrary ''ChoosePositionResult
derive makeArbitrary ''ReorderCellResult
derive makeArbitrary ''Health
derive makeArbitrary ''QueryResult
derive makeArbitrary ''StartResult
derive makeArbitrary ''MoveResult

isSecret :: Move -> Bool
isSecret (ChoosePosition _) = True
isSecret (ReorderCell _)    = True
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

prop_show_choose_position =
        forAll arbitraryChoose $ \m1 ->
        forAll arbitraryChoose $ \m2 ->
        show m1 == show m2
    where arbitraryChoose = (liftM ChoosePosition) arbitrary

prop_show_reorder_cell =
        forAll arbitraryReorder $ \m1 ->
        forAll arbitraryReorder $ \m2 ->
        show m1 == show m2
    where arbitraryReorder = (liftM ReorderCell) arbitrary

test_parse_reorder_cell = do
    assertEqual
        (Right $ ReorderCell $ Pos 1 4)
        $ parseMove "reorder 1 4"

prop_show_move_result :: MoveResult -> Bool
prop_show_move_result = (0 <) . length . show
