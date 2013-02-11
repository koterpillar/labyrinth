{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestLabyrinth.Show (htf_thisModulesTests) where

import Labyrinth

import Control.Monad

import Text.Parsec

import Test.Framework
import qualified Test.HUnit as HU
import Test.QuickCheck

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

instance Arbitrary Direction where
    arbitrary = elements [ L, R, U, D ]

instance Arbitrary MoveDirection where
    arbitrary = oneof [ return Next
                      , liftM Towards arbitrary
                      ]

instance Arbitrary Action where
    arbitrary = oneof [ liftM Go arbitrary
                      , liftM Shoot arbitrary
                      , liftM Grenade arbitrary
                      ]

instance Arbitrary Move where
    arbitrary = liftM Move arbitrary

instance Arbitrary CellTypeResult where
    arbitrary = elements [ LandR
                         , ArmoryR
                         , HospitalR
                         , PitR
                         , RiverR
                         , RiverDeltaR
                         ]

instance Arbitrary GoResult where
    arbitrary = oneof [ liftM5 Went arbitrary arbitrary arbitrary arbitrary arbitrary
                      , return HitWall
                      ]

instance Arbitrary ShootResult where
    arbitrary = elements [ ShootOK
                         , Scream
                         , NoBullets
                         , Forbidden
                         ]

instance Arbitrary GrenadeResult where
    arbitrary = elements [ GrenadeOK, NoGrenades ]

instance Arbitrary ActionResult where
    arbitrary = oneof [ liftM GoR arbitrary
                      , liftM ShootR arbitrary
                      , liftM GrenadeR arbitrary
                      ]

instance Arbitrary MoveResult where
    arbitrary = liftM MoveRes arbitrary

prop_show_parse_move :: Move -> Bool
prop_show_parse_move m = parsed == m
    where
        parseResult = parseMove $ show m
        parsed = case parseResult of
            Right x -> x
            Left y -> error y

prop_show_move_result :: MoveResult -> Bool
prop_show_move_result = (0 <) . length . show
