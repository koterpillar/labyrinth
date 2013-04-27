{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import {-@ HTF_TESTS @-} TestLabyrinth
import {-@ HTF_TESTS @-} TestLabyrinth.Generate
import {-@ HTF_TESTS @-} TestLabyrinth.Move.ChoosePosition
import {-@ HTF_TESTS @-} TestLabyrinth.Move.Conditional
import {-@ HTF_TESTS @-} TestLabyrinth.Move.Grenade
import {-@ HTF_TESTS @-} TestLabyrinth.Move.Movement
import {-@ HTF_TESTS @-} TestLabyrinth.Move.Query
import {-@ HTF_TESTS @-} TestLabyrinth.Move.Say
import {-@ HTF_TESTS @-} TestLabyrinth.Move.Shoot
import {-@ HTF_TESTS @-} TestLabyrinth.Move.Surrender
import {-@ HTF_TESTS @-} TestLabyrinth.ShowLabyrinth
import {-@ HTF_TESTS @-} TestLabyrinth.ShowMove

main = htfMain htf_importedTests
