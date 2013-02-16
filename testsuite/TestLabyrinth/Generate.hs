{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestLabyrinth.Generate (htf_thisModulesTests) where

import Control.Monad

import Peeker

import System.Random

import Labyrinth

import Test.Framework

instance Arbitrary Labyrinth where
    arbitrary = liftM (fst . generateLabyrinth 5 6 3 . mkStdGen) arbitrary

isCellType :: CellTypeResult -> Cell -> Bool
isCellType ct = (ct ==) . ctResult . getP ctype

countByType :: CellTypeResult -> [Cell] -> Int
countByType ct = length . filter (isCellType ct)

prop_no_unique_cells :: Labyrinth -> Bool
prop_no_unique_cells l = and $ map (\ct -> noUnique ct cells) allTypes
    where cells = allCells l
          noUnique ct = (1 /=) . (countByType ct)
          allTypes = [ ArmoryR
                     , HospitalR
                     , PitR
                     , RiverR
                     , RiverDeltaR
                     ]

prop_has_required_types :: Labyrinth -> Bool
prop_has_required_types l = and $ map (\ct -> typeExists ct cells) requiredTypes
    where cells = allCells l
          typeExists ct = (0 <) . (countByType ct)
          requiredTypes = [ ArmoryR
                          , HospitalR
                          , LandR
                          ]
