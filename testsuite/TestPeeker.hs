{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# Language TemplateHaskell #-}

module TestPeeker (htf_thisModulesTests) where

import Peeker

import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Map as M

import Test.Framework

data Wrap2 a = Wrap2 (Wrap a)
             deriving (Eq, Show)

data Wrap a = Wrap a
            deriving (Eq, Show)

data Flip = Foo | Bar
            deriving (Eq, Show)

data Fruit = Apple | Orange
             deriving (Eq, Show)

data Animal = Cat | Fox
              deriving (Eq, Show)

data Rec = RecC { fruit_  :: Fruit
                , animal_ :: Animal
                }
         | RecC2 { number_ :: Int
                 }
         | RecC3 Bool Bool
         deriving (Eq, Show)

derivePeek ''Rec

p1 :: Peek (Wrap2 a) (Wrap a)
p1 (Wrap2 x) = (x, Wrap2)

p2 :: Peek (Wrap a) a
p2 (Wrap x) = (x, Wrap)

test_composition = do
    assertEqual
        Foo $
        getP (p1 ~> p2) $ Wrap2 $ Wrap Foo
    assertEqual
        (Wrap2 (Wrap Bar)) $
        updP (p1 ~> p2) (Wrap2 $ Wrap Foo) Bar

test_getter = do
    assertEqual
        Foo $
        getP p2 $ Wrap Foo

test_updater = do
    assertEqual
        (Wrap Bar) $
        updP p2 (Wrap Foo) Bar

test_state = do
    assertEqual
        Foo $
        evalState (getS p2) (Wrap Foo)
    assertEqual
        (Wrap Bar) $
        execState (updS p2 Bar) (Wrap Foo)
    let plusOne :: State Int String
        plusOne = do
        x <- get
        put $ x + 1
        return "inner result"
    assertEqual
        ("inner result", Wrap 2) $
        runState (stateS p2 plusOne) (Wrap 1)

test_reader = do
    assertEqual
        Foo $
        runReader (askS p2) (Wrap Foo)

test_lift = do
    assertEqual
        Foo $
        getP liftP Foo
    assertEqual
        Bar $
        updP liftP Foo Bar

test_list = do
    let lst = [10,20,30,40]
    assertEqual
        30 $
        getP (listP 2) lst
    assertEqual
        [10,20,99,40] $
        updP (listP 2) lst 99

test_map = do
    let map = M.fromList [(1, 2), (3, 4)]
    assertEqual
        2 $
        getP (mapP 1) map
    assertEqual
        (M.fromList [(1, 10), (3, 4)]) $
        updP (mapP 1) map 10

test_template = do
    let rec = RecC Apple Cat
    let rec2 = RecC2 10
    assertEqual
        Apple $
        getP fruit rec
    assertEqual
        Cat $
        getP animal rec
    assertEqual
        10 $
        getP number rec2
    assertEqual
        (RecC Orange Cat) $
        updP fruit rec Orange
    assertEqual
        (RecC Apple Fox) $
        updP animal rec Fox
    assertEqual
        (RecC2 5) $
        updP number rec2 5
