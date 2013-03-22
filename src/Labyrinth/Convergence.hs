module Labyrinth.Convergence where

import Control.Monad.Reader

import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Map ((!))

import Labyrinth
import Labyrinth.Generate

type Distribution = M.Map Position Double

converge :: Reader Labyrinth Distribution
converge = do
    initial <- uniformDist
    w <- asks labWidth
    h <- asks labHeight
    let n = w * h `div` 4
    res <- foldM (\dist _ -> convergeStep dist) initial [1..n]
    return res

lengthProb :: [a] -> Double
lengthProb = (1 /) . fromIntegral . length

uniformDist :: Reader Labyrinth Distribution
uniformDist = do
    pos <- asks allPositions
    return $ M.fromList $ zip pos $ repeat $ lengthProb pos

convergeStep :: Distribution -> Reader Labyrinth Distribution
convergeStep dist = do
    pos <- asks allPositions
    posReach <- mapM reachable pos
    let dists = zipWith spread (map (dist !) pos) posReach
    return $ foldr1 (M.unionWith (+)) dists

spread :: Double -> [Position] -> Distribution
spread prob pos = M.fromList $ zip pos $ repeat $ prob * lengthProb pos

maxBy :: (Position -> Int) -> Distribution -> Int
maxBy prop = prop . maximumBy (compare `on` prop) . M.keys

showGrid :: Distribution -> String
showGrid g = intercalate "\n" $ (flip map) [0..maxY] $ \y -> showGridLine y g
    where maxY = maxBy pY g

showGridLine :: Int -> Distribution -> String
showGridLine y g = intercalate " " $ (flip map) [0..maxX] $ \x -> showGridPos x y g
    where maxX = maxBy pX g

showGridPos :: Int -> Int -> Distribution -> String
showGridPos x y = pad 2 ' ' . show . round . (100 *) . (M.findWithDefault 0 (Pos x y))

pad :: Int -> a -> [a] -> [a]
pad n c l = replicate d c ++ l where d = max 0 $ n - length l
