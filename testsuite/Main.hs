{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import {-@ HTF_TESTS @-} TestLabyrinth
import {-@ HTF_TESTS @-} TestLabyrinth.Show
import {-@ HTF_TESTS @-} TestLabyrinth.Generate
import {-@ HTF_TESTS @-} TestPeeker

main = htfMain htf_importedTests
