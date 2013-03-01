module Labyrinth.Show where

import Labyrinth.Map
import Labyrinth.Move

import Control.Monad.Reader
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

instance Show Treasure where
    show TrueTreasure = "true treasure"
    show FakeTreasure = "fake treasure"

instance Show Health where
    show Healthy = "healthy"
    show Wounded = "wounded"
    show Dead    = "dead"

instance Show Player where
    show p = execWriter $ (flip runReaderT) p $ do
        tell "Player "
        pos <- askS position
        tell $ show pos
        tell ", "
        b <- askS pbullets
        tell $ show b
        tell "B"
        tell ", "
        g <- askS pgrenades
        tell $ show g
        tell "G"
        h <- askS phealth
        when (h /= Healthy) $ do
            tell ", "
            tell $ show h
        f <- askS pfell
        when f $ do
            tell ", fallen"

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
showCurrentPlayer l = ["Current player: " ++ show (getP currentTurn l)]

showItems :: Labyrinth -> [String]
showItems = concat . map showCellItemsOn . allPosCells
    where showCellItemsOn (p, c) = if itemStr == "" then [] else [showStr]
              where itemStr = showCellItems c
                    showStr = show p ++ ": " ++ itemStr

showCellItems :: Cell -> String
showCellItems c = intercalate ", " $ execWriter $ (flip runReaderT) c $ do
    b <- askS cbullets
    when (b > 0) $ tell [show b ++ "B"]
    g <- askS cgrenades
    when (g > 0) $ tell [show g ++ "G"]
    t <- askS ctreasures
    tell $ map show t

showStatus :: Labyrinth -> [String]
showStatus l = execWriter $ (flip runReaderT) l $ do
    pc <- askS positionsChosen
    when (not pc) $ tell ["Positions not chosen"]
    end <- askS gameEnded
    when end $ tell ["Game ended"]

instance Show Labyrinth where
    show l = intercalate "\n" $ concat parts
        where parts = map ($ l) [ showMap
                                , const [""]
                                , showPlayers
                                , showCurrentPlayer
                                , showItems
                                , showStatus
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
    show (Move [])          = "skip"
    show (Move acts)        = intercalate ", " $ map show acts
    show (ChoosePosition _) = "[choose position]"
    show (ReorderCell _)    = "[reorder cell]"

instance Show CellTypeResult where
    show LandR = "land"
    show ArmoryR = "armory"
    show HospitalR = "hospital"
    show PitR = "pit"
    show RiverR = "river"
    show RiverDeltaR = "delta"

instance Show CellEvents where
    show r = execWriter $ do
            let transported = getP transportedTo r
            when (isJust transported) $ do
                tell ", was transported to "
                tell $ show $ fromJust $ transported
            let b = getP foundBullets r
            let g = getP foundGrenades r
            let t = getP foundTreasures r
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

instance Show ActionResult where
    show (GoR (HitWall cr)) = "hit a wall" ++ show cr
    show (GoR (Went ct cr)) = "went onto " ++ show ct ++ show cr
    show (GoR went@WentOutside{}) = execWriter $ do
        tell "went outside"
        let tr = getP treasureResult went
        case tr of
            Just TurnedToAshesR -> tell ", treasure turned to ashes"
            Just TrueTreasureR  -> tell " with a true treasure - victory"
            Nothing             -> return ()
        return ()
    show (GoR InvalidMovement) = "invalid movement"

    show (ShootR ShootOK)   = "ok"
    show (ShootR Scream)    = "a scream is heard"
    show (ShootR NoBullets) = "no bullets"
    show (ShootR Forbidden) = "shooting forbidden"

    show (GrenadeR GrenadeOK)  = "ok"
    show (GrenadeR NoGrenades) = "no grenades"

instance Show StartResult where
    show (StartR pi ct cr) = "player " ++ show pi
                          ++ " started at " ++ show ct ++ show cr

instance Show ChoosePositionResult where
    show ChosenOK          = "position chosen"
    show (AllChosenOK pos) = "game started; " ++ (intercalate "; " $ map show pos)
    show ChooseAgain       = "positions chosen invalid, choose again"

instance Show ReorderCellResult where
    show (ReorderOK ct cr) = "cell re-ordered, went onto " ++ show ct ++ show cr
    show ReorderForbidden  = "cannot re-order cell"

instance Show MoveResult where
    show (MoveRes [])          = "ok"
    show (MoveRes rs)          = intercalate ", " $ map show rs
    show WrongTurn             = "wrong turn"
    show InvalidMove           = "invalid move"
    show (ChoosePositionR cpr) = show cpr
    show (ReorderCellR cr)     = show cr
