{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestLabyrinth where

import Labyrinth

import Control.Lens
import Control.Monad
import Control.Monad.State

import Test.Framework
import TestLabyrinth.Common
import TestLabyrinth.Move.Common

test_advance = do
    assertEqual
        (Pos 0 1) $
        advance (Pos 0 0) D

test_outside = do
    let l = empty_labyrinth
    mapM_ (\p -> assertBool $ isOutside p l) [ Pos (-1) (-1)
                                             , Pos 0 (-1)
                                             , Pos (-1) 0
                                             , Pos 6 0
                                             , Pos 0 5
                                             , Pos 6 5
                                             , Pos 0 (-2)
                                             , Pos (-2) 0
                                             , Pos 9 0
                                             , Pos 0 9
                                             , Pos 9 9
                                             ]

test_inside = do
    let l = empty_labyrinth
    mapM_ (\p -> assertBool $ isInside p l) [ Pos 0 0
                                            , Pos 0 1
                                            , Pos 1 0
                                            , Pos 5 0
                                            , Pos 0 4
                                            , Pos 5 4
                                            ]

test_way_outside = do
    let l = empty_labyrinth
    mapM_ (\p -> assertBool $ wayOutside p l) [ Pos 0 (-2)
                                              , Pos (-2) 0
                                              , Pos (-2) (-2)
                                              , Pos 7 0
                                              , Pos 0 6
                                              , Pos 7 6
                                              ]
    mapM_ (\p -> assertBool $ not $ wayOutside p l) [ Pos (-1) (-1)
                                                    , Pos 0 (-1)
                                                    , Pos (-1) 0
                                                    , Pos 6 0
                                                    , Pos 0 5
                                                    , Pos 6 5
                                                    , Pos 0 0
                                                    , Pos 2 2
                                                    , Pos 5 4
                                                    , Pos 5 0
                                                    , Pos 0 4
                                                    ]

test_wrong_turn = do
    assertMoveUpdates
        empty_labyrinth
        1
        (Move [goTowards D])
        (MoveRes [WrongTurn])
        $ return ()
    let endgame = applyState empty_labyrinth $ do
        gameEnded .= True
    assertMoveUpdates'
        endgame
        (Move [goTowards D])
        (MoveRes [WrongTurn])
        $ return ()

test_combined = do
    assertMoveUpdates'
        walled_labyrinth
        (Move [Grenade R, goTowards R])
        (MoveRes [GrenadeR GrenadeOK, GoR $ Went LandR noEvents])
        $ do
            (player 0 . position) .= Pos 1 0
            (player 0 . pgrenades) .= 2
            wall (Pos 0 0) R .= NoWall
            currentTurn .= 1

test_invalid = do
    assertMoveUpdates'
        walled_labyrinth
        (Move [goTowards R, goTowards R])
        (MoveRes [InvalidMove])
        $ return ()
