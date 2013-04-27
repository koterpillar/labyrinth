{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestLabyrinth.Move.Say where

import Labyrinth

import Control.Lens

import Test.Framework
import TestLabyrinth.Common
import TestLabyrinth.Move.Common

lab = applyState empty_labyrinth $ do
    (player 0 . pgrenades) .= 2
    (player 1 . ptreasure) .= Just TrueTreasure

test_say = do
    assertMoveUpdates lab 0
        (Say "hello")
        (MoveRes [])
        $ return ()
