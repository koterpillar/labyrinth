{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestLabyrinth.Move.Query where

import Labyrinth

import Control.Lens

import Test.Framework
import TestLabyrinth.Common
import TestLabyrinth.Move.Common

lab = applyState empty_labyrinth $ do
    (player 0 . pgrenades) .= 2
    (player 1 . ptreasure) .= Just TrueTreasure

test_query = do
    assertMoveUpdates lab 0
        (Query [BulletCount, GrenadeCount])
        (MoveRes [QueryR $ BulletCountR 3, QueryR $ GrenadeCountR 2])
        $ return ()

test_query_another_move = do
    assertMoveUpdates lab 1
        (Query [TreasureCarried, PlayerHealth])
        (MoveRes [QueryR $ TreasureCarriedR True, QueryR $ HealthR Healthy])
        $ return ()
