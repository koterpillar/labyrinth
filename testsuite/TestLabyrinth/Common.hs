{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestLabyrinth.Common where

import Labyrinth

import Control.Lens
import Control.Monad.State

import Test.Framework
import qualified Test.HUnit as HU

assertShowEquals :: (Show a) => String -> a -> HU.Assertion
assertShowEquals message move = assertEqual message $ show move

w = 6
h = 5

defaultParams :: Int -> LabyrinthParams
defaultParams = LabyrinthParams w h

empty_labyrinth = applyState (emptyLabyrinth $ defaultParams 2) $ do
    (player 0 . position) .= Pos 0 0
    (player 1 . position) .= Pos 2 2
    positionsChosen .= True

applyState = flip execState

walled_labyrinth = applyState empty_labyrinth $ do
    forM_ [0..w-1] $
        \x -> forM_ [0..h-2] $
            \y -> do
                wall (Pos x y) D .= Wall
    forM_ [0..w-2] $
        \x -> forM_ [0..h-1] $
            \y -> do
                wall (Pos x y) R .= Wall

interesting_labyrinth = applyState empty_labyrinth $ do
    (player 0 . position) .= Pos 1 1
    (player 1 . position) .= Pos 3 3
    (cell (Pos 1 0) . ctype) .= Armory
    (cell (Pos 2 0) . ctype) .= River D
    (cell (Pos 4 0) . ctype) .= Pit 1
    (cell (Pos 2 1) . ctype) .= River D
    (cell (Pos 4 1) . ctype) .= Pit 2
    (cell (Pos 1 2) . ctype) .= RiverDelta
    (cell (Pos 2 2) . ctype) .= River L
    (cell (Pos 3 2) . ctype) .= Pit 0
    (cell (Pos 2 4) . ctype) .= Hospital
    wall (Pos 3 0) U .= Wall
    wall (Pos 2 2) R .= Wall
    wall (Pos 3 2) R .= Wall
    wall (Pos 0 3) L .= NoWall
    wall (Pos 4 4) D .= NoWall
    (cell (Pos 5 3) . ctreasures) .= [TrueTreasure]
    (cell (Pos 1 3) . ctreasures) .= [FakeTreasure]
