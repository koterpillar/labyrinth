{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestLabyrinth.Generate (htf_thisModulesTests) where

import Control.Lens
import Control.Monad.Reader

import Data.List
import qualified Data.Map as M

import System.Random

import Labyrinth
import Labyrinth.Reachability

import Test.Framework
import Test.QuickCheck (conjoin, printTestCase, Property)

import TestLabyrinth.Common

instance Arbitrary Labyrinth where
    arbitrary = liftM (fst . generateLabyrinth (defaultParams 3) . mkStdGen) arbitrary

newtype SimpleLabyrinth = SimpleLabyrinth Labyrinth
                          deriving (Eq, Show)

instance Arbitrary SimpleLabyrinth where
    arbitrary = liftM (SimpleLabyrinth . fst . generateLabyrinth (simpleParams 3) . mkStdGen) arbitrary

isCellType :: CellTypeResult -> Cell -> Bool
isCellType ct = (ct ==) . ctResult . view ctype

countByType :: CellTypeResult -> [Cell] -> Int
countByType ct = length . filter (isCellType ct)

labyrinthTests :: [(String, Labyrinth -> Bool)]
labyrinthTests = [ ("no unique cells", no_unique_cells)
                 , ("has required types", has_required_types)
                 , ("has true treasure", has_true_treasure)
                 , ("enough fake treasures", enough_fake_treasures)
                 , ("no treasures together", no_treasures_together)
                 , ("treasures on land", treasures_on_land)
                 , ("enough exits", enough_exits)
                 , ("no walls in rivers", no_walls_in_rivers)
                 , ("armory reachable", armory_reachable)
                 ]

testAll :: Arbitrary a => [(String, a -> Bool)] -> a -> Property
testAll tests x =
    conjoin [printTestCase ("failed: " ++ msg) $ tst x | (msg, tst) <- tests]

prop_good_labyrinth :: Labyrinth -> Property
prop_good_labyrinth = testAll labyrinthTests

prop_good_simple :: SimpleLabyrinth -> Property
prop_good_simple (SimpleLabyrinth l) = testAll simpleLabyrinthTests l
    where simpleLabyrinthTests = labyrinthTests
                              ++ [ ("no rivers", no_rivers)
                                 , ("no pits", no_pits)
                                 ]

no_pits :: Labyrinth -> Bool
no_pits l = countByType PitR (allCells l) == 0

no_rivers :: Labyrinth -> Bool
no_rivers l = countByType RiverR (allCells l) == 0

no_unique_cells :: Labyrinth -> Bool
no_unique_cells l = and $ map (\ct -> noUnique ct cells) allTypes
    where cells = allCells l
          noUnique ct = (1 /=) . (countByType ct)
          allTypes = [ ArmoryR
                     , HospitalR
                     , PitR
                     , RiverR
                     , RiverDeltaR
                     ]

has_required_types :: Labyrinth -> Bool
has_required_types l = and $ map (\ct -> typeExists ct cells) requiredTypes
    where cells = allCells l
          typeExists ct = (0 <) . (countByType ct)
          requiredTypes = [ ArmoryR
                          , HospitalR
                          , LandR
                          ]

has_true_treasure :: Labyrinth -> Bool
has_true_treasure = (1 ==) . length . filter hasTrueTreasure . allCells
    where hasTrueTreasure = ([TrueTreasure] ==) . view ctreasures

enough_fake_treasures :: Labyrinth -> Bool
enough_fake_treasures l = fakeTreasureCount >= 1 && fakeTreasureCount <= (playerCount l)
    where fakeTreasureCount = length $ filter hasFakeTreasure $ allCells l
          hasFakeTreasure = ([FakeTreasure] ==) . view ctreasures

no_treasures_together :: Labyrinth -> Bool
no_treasures_together = and . map ((1 >=) . treasureCount) . allCells
    where treasureCount = length . view ctreasures

treasures_on_land :: Labyrinth -> Bool
treasures_on_land = and . map isLand . filter hasTreasures . allCells
    where isLand = isCellType LandR
          hasTreasures = (0 <) . length . view ctreasures

enough_exits :: Labyrinth -> Bool
enough_exits l = (2 <=) $ length $ filter isExit $ outerPos l
    where isExit (p, d) = l ^. wall p d /= HardWall

no_walls_in_rivers :: Labyrinth -> Bool
no_walls_in_rivers l = and $ map noWall $ filter isRiver $ allPosCells l
    where isRiver (_, c) = isRiver' $ c ^. ctype
          isRiver' (River _) = True
          isRiver' _         = False
          noWall (p, c) = l ^. wall p d == NoWall
              where d = c ^?! (ctype . riverDirection)

armory_reachable l = allPositions l == (sort $ M.keys $ reachConvergeU l)
