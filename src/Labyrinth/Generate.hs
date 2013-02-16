module Labyrinth.Generate (generateLabyrinth) where

import Labyrinth.Map

import System.Random

generateLabyrinth :: (RandomGen g) => Int -> Int -> Int -> g -> (Labyrinth, g)
generateLabyrinth w h p g = (l, g)
    where l = emptyLabyrinth w h $ take p $ repeat $ Pos 0 0
