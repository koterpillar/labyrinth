{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestLabyrinth.Generate (htf_thisModulesTests) where

import Peeker

import System.Random

import Labyrinth

import Test.Framework

generateFromSeed :: Int -> Labyrinth
generateFromSeed = fst . generateLabyrinth 5 6 3 . mkStdGen

prop_no_unique_cells :: Int -> Bool
prop_no_unique_cells = (1 /=) . length . (filter is_land) . allCells . generateFromSeed
    where is_land = (Land ==) . getP ctype
