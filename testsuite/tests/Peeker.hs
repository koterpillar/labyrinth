{-# Language TemplateHaskell #-}

import Peeker

import Test.HUnit

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

data Rec = Rec { fruit_  :: Fruit
               , animal_ :: Animal
               }
               deriving (Eq, Show)

derivePeek ''Rec

main = runTestTT tests

tests = TestList [ test_getter
                 , test_updater
                 , test_composition
                 , test_lift
                 , test_template
                 ]

p1 :: Peek (Wrap2 a) (Wrap a)
p1 (Wrap2 x) = (x, Wrap2)

p2 :: Peek (Wrap a) a
p2 (Wrap x) = (x, Wrap)

test_composition = TestCase $ do
    assertEqual "get via composition"
        Foo $
        get (p1 ~> p2) $ Wrap2 $ Wrap Foo
    assertEqual "set via composition"
        (Wrap2 (Wrap Bar)) $
        upd (p1 ~> p2) (Wrap2 $ Wrap Foo) Bar

test_getter = TestCase $ do
    assertEqual "get value"
        Foo $
        get p2 $ Wrap Foo

test_updater = TestCase $ do
    assertEqual "update value"
        (Wrap Bar) $
        upd p2 (Wrap Foo) Bar

test_lift = TestCase $ do
    assertEqual "getting lifted value"
        Foo $
        get liftP Foo
    assertEqual "updating lifted value"
        Bar $
        upd liftP Foo Bar

test_template = TestCase $ do
    let rec = Rec Apple Cat
    assertEqual "get first derived value"
        Apple $
        get fruit rec
    assertEqual "get second derived value"
        Cat $
        get animal rec
    assertEqual "put derived value"
        (Rec Orange Cat) $
        upd fruit rec Orange
    assertEqual "put second derived value"
        (Rec Apple Fox) $
        upd animal rec Fox
