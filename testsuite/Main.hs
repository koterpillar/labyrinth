{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import {-@ HTF_TESTS @-} TestLabyrinth
import {-@ HTF_TESTS @-} TestLabyrinth.Generate
import {-@ HTF_TESTS @-} TestLabyrinth.Move.Shoot
import {-@ HTF_TESTS @-} TestLabyrinth.ShowLabyrinth
import {-@ HTF_TESTS @-} TestLabyrinth.ShowMove
import {-@ HTF_TESTS @-} TestPeeker

main = htfMain htf_importedTests
