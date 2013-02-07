module Labyrinth.Show where

import Labyrinth.Map
import Labyrinth.Move

import Control.Monad.Writer

import Data.List
import Data.Maybe

import Peeker

instance Show CellType where
    show Land       = "."
    show Armory     = "A"
    show Hospital   = "H"
    show (Pit i)    = show (i + 1)
    show (River L)  = "<"
    show (River R)  = ">"
    show (River U)  = "^"
    show (River D)  = "v"
    show RiverDelta = "O"

instance Show Cell where
    show c = show (ctype_ c) ++ " "

instance Show Player where
    show p = "Player "
          ++ show (position_ p)
          ++ ", "
          ++ show (pbullets_ p) ++ "B"
          ++ ", "
          ++ show (pgrenades_ p) ++ "G"

showH :: Wall -> String
showH NoWall   = "  "
showH Wall     = "--"
showH HardWall = "=="

showV :: Wall -> String
showV NoWall   = " "
showV Wall     = "|"
showV HardWall = "X"

showWallLine :: Labyrinth -> Int -> String
showWallLine l y = mk ++ intercalate mk ws ++ mk
    where mk = "+"
          w  = labWidth l
          ws = map (\x -> showH $ getP (wallH (Pos x y)) l) [0..w - 1]

showCellLine :: Labyrinth -> Int -> String
showCellLine l y = concat (map (\x -> showVWall l (Pos x y) ++ showCell l (Pos x y)) [0..w - 1])
                       ++ showVWall l (Pos w y)
                   where w = labWidth l
                         showVWall :: Labyrinth -> Position -> String
                         showVWall l p = showV $ getP (wallV p) l
                         showCell :: Labyrinth -> Position -> String
                         showCell l p = show $ getP (cell p) l

showMap :: Labyrinth -> [String]
showMap l = firstLines ++ [lastLine]
    where h = labHeight l
          showLine l i = [showWallLine l i, showCellLine l i]
          firstLines = concat $ map (showLine l) [0..h - 1]
          lastLine = showWallLine l h

showPlayers :: Labyrinth -> [String]
showPlayers l = map (uncurry showPlayer) $ zip (getP players l) [0..]
    where showPlayer p i = (show i) ++ ": " ++ (show p)

showCurrentPlayer :: Labyrinth -> [String]
showCurrentPlayer l = ["Current player: " ++ show (getP currentPlayer l)]

instance Show Labyrinth where
    show l = intercalate "\n" $ foldr1 (++) parts
        where parts = map ($ l) [ showMap
                                , const [""]
                                , showPlayers
                                , showCurrentPlayer
                                ]

instance Show Direction where
    show L = "left"
    show R = "right"
    show U = "up"
    show D = "down"

instance Show MoveDirection where
    show (Towards d) = show d
    show Next = "next"

instance Show Action where
    show (Go d) = "go " ++ (show d)
    show (Shoot d) = "shoot " ++ (show d)
    show (Grenade d) = "grenade " ++ (show d)

instance Show Move where
    show (Move []) = "skip"
    show (Move acts) = intercalate ", " $ map show acts

instance Show CellTypeResult where
    show LandR = "land"
    show ArmoryR = "armory"
    show HospitalR = "hospital"
    show PitR = "pit"
    show RiverR = "river"
    show RiverDeltaR = "delta"

instance Show ActionResult where
    show (GoR HitWall) = "hit a wall"
    show (GoR went) = execWriter $ do
            tell "went onto "
            tell $ show $ getP onto went
            let transported = getP transportedTo went
            when (isJust transported) $ do
                tell ", was transported to "
                tell $ show $ fromJust $ transported
            let b = getP foundBullets went
            let g = getP foundGrenades went
            let t = getP foundTreasures went
            let found = b > 0 || g > 0 || t > 0
            when found $ do
                tell ", found "
                tell $
                    commaList $
                    map (uncurry pluralize) $
                    filter ((0 <) . fst) $
                    [(b, "bullet"), (g, "grenade"), (t, "treasure")]
            return ()
        where pluralize 1 str = "a " ++ str
              pluralize n str = (show n) ++ " " ++ str ++ "s"
              commaList [] = ""
              commaList [x] = x
              commaList xs = intercalate ", " (take (n - 1) xs)
                          ++ " and " ++ xs !! (n - 1)
                          where n = length xs

instance Show MoveResult where
    show (MoveRes []) = "ok"
    show (MoveRes rs) = intercalate ", " $ map show rs
