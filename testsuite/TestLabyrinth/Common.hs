{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestLabyrinth.Common where

import Labyrinth

import Peeker

import Control.Monad.State

import Test.Framework
import qualified Test.HUnit as HU

assertShowEquals :: (Show a) => String -> a -> HU.Assertion
assertShowEquals message move = assertEqual message $ show move

w = 6
h = 5

empty_labyrinth = applyState (emptyLabyrinth w h 2) $ do
    updS (player 0 ~> position) $ Pos 0 0
    updS (player 1 ~> position) $ Pos 2 2
    updS positionsChosen True

applyState = flip execState

walled_labyrinth = applyState empty_labyrinth $ do
    forM_ [0..w-1] $
        \x -> forM_ [0..h-2] $
            \y -> do
                updS (wall (Pos x y) D) Wall
    forM_ [0..w-2] $
        \x -> forM_ [0..h-1] $
            \y -> do
                updS (wall (Pos x y) R) Wall

interesting_labyrinth = applyState empty_labyrinth $ do
    updS (player 0 ~> position) (Pos 1 1)
    updS (player 1 ~> position) (Pos 3 3)
    updS (cell (Pos 1 0) ~> ctype) Armory
    updS (cell (Pos 2 0) ~> ctype) $ River D
    updS (cell (Pos 4 0) ~> ctype) $ Pit 1
    updS (cell (Pos 2 1) ~> ctype) $ River D
    updS (cell (Pos 4 1) ~> ctype) $ Pit 2
    updS (cell (Pos 1 2) ~> ctype) RiverDelta
    updS (cell (Pos 2 2) ~> ctype) $ River L
    updS (cell (Pos 3 2) ~> ctype) $ Pit 0
    updS (cell (Pos 2 4) ~> ctype) Hospital
    updS (wall (Pos 3 0) U) Wall
    updS (wall (Pos 2 2) R) Wall
    updS (wall (Pos 3 2) R) Wall
    updS (wall (Pos 0 3) L) NoWall
    updS (wall (Pos 4 4) D) NoWall
    updS (cell (Pos 5 3) ~> ctreasures) $ [TrueTreasure]
    updS (cell (Pos 1 3) ~> ctreasures) $ [FakeTreasure]
