module Labyrinth.Reachability where

import Control.Lens
import Control.Monad.Reader

import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid

import Labyrinth.Map

type PositionMap a = M.Map Position a

type Connectivity = PositionMap [Position]

type Distribution = PositionMap Double

type Reachability = PositionMap Bool

nextCell :: Position -> Reader Labyrinth Position
nextCell pos = do
    ct <- view $ cell pos . ctype
    case ct of
        River d -> return $ advance pos d
        Pit i   -> do
                       npits <- asks pitCount
                       let i' = (i + 1) `mod` npits
                       asks (pit i')
        _       -> return pos

-- A list of positions player can go from a given cell
reachable :: Position -> Reader Labyrinth [Position]
reachable pos = do
    dirs <- filterM (liftM (NoWall ==) . view . wall pos) allDirections
    let npos = pos : map (advance pos) dirs
    npos' <- filterM (asks . isInside) npos
    npos'' <- forM npos' nextCell
    return $ nub npos''

connectivity :: Labyrinth -> Connectivity
connectivity = runReader $ do
    pos <- asks allPositions
    posReach <- mapM reachable pos
    return $ M.fromList $ zip pos posReach

insertAppend :: (Ord k) => k -> v -> M.Map k [v] -> M.Map k [v]
insertAppend k v = M.alter (addToList v) k
    where addToList v = Just . (v :) . fromMaybe []

inverse :: (Ord a, Ord b) => M.Map a [b] -> M.Map b [a]
inverse = M.foldWithKey insertAll M.empty
    where insertAll k vs m = foldr (flip insertAppend k) m vs

foldConcat :: (Monoid v) => M.Map k [v] -> M.Map k v
foldConcat = M.map mconcat

distribute :: (Ord k, Monoid v) => M.Map k [k] -> M.Map k v -> M.Map k v
distribute dist = foldConcat . M.foldWithKey insertAll M.empty
    where insertAll k v m = foldr (flip insertAppend v) m k2s
              where k2s = M.findWithDefault [] k dist

distributeN :: (Ord k, Monoid v) => Int -> M.Map k [k] -> M.Map k v -> M.Map k v
distributeN n dist init = foldr distribute init $ replicate n dist

distributeU :: (Ord k, Monoid v, Eq v) => M.Map k [k] -> M.Map k v -> M.Map k v
distributeU dist init =
    if next == init then init else distributeU dist next
    where next = distribute dist init

normalize :: (Fractional v) => M.Map k v -> M.Map k v
normalize m = M.map norm m
    where norm = (/ s)
          s = sum $ M.elems m

converge :: Int -> Labyrinth -> Distribution
converge n l = normalize $ M.map getSum $ distributeN n conn init
    where conn = connectivity l
          pos = allPositions l
          init = uniformBetween (Sum 1) pos

reachConverge :: Int -> Labyrinth -> Reachability
reachConverge n l = M.map getAny $ distributeN n conn init
    where conn = inverse $ connectivity l
          pos = armories l
          init = uniformBetween (Any True) pos

reachConvergeU :: Labyrinth -> Reachability
reachConvergeU l = M.map getAny $ distributeU conn init
    where conn = inverse $ connectivity l
          pos = armories l
          init = uniformBetween (Any True) pos

uniformBetween :: a -> [Position] -> PositionMap a
uniformBetween x pos = M.fromList $ zip pos $ repeat x

armoriesDist :: Reader Labyrinth Reachability
armoriesDist = liftM (uniformBetween True) $ asks armories

maxKeyBy :: (Ord n) => (k -> n) -> M.Map k a -> n
maxKeyBy prop = maximum . M.keys . M.mapKeys prop

showReach :: Reachability -> String
showReach = showGrid showReachValue
    where showReachValue = pad 2 ' ' . showR . fromMaybe False
          showR True  = "*"
          showR False = "."

showDist :: Distribution -> String
showDist = showGrid showDistValue
    where showDistValue = pad 2 ' ' . show . round . (100 *) . fromMaybe 0

showGrid :: (Maybe a -> String) -> PositionMap a -> String
showGrid s g = intercalate "\n" $ flip map [0..maxY] $ showGridLine s g
    where maxY = maxKeyBy pY g

showGridLine :: (Maybe a -> String) -> PositionMap a -> Int -> String
showGridLine s g y = unwords $ flip map [0..maxX] $ showGridPos s g y
    where maxX = maxKeyBy pX g

showGridPos :: (Maybe a -> String) -> PositionMap a -> Int -> Int -> String
showGridPos s g y x = s $ M.lookup (Pos x y) g

pad :: Int -> a -> [a] -> [a]
pad n c l = replicate d c ++ l where d = max 0 $ n - length l
