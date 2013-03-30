module Labyrinth.Show where

import Labyrinth.Map
import Labyrinth.Move

import Control.Lens hiding (Action)
import Control.Monad.Reader
import Control.Monad.Writer

import Data.List
import Data.Maybe

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
    show c = show (_ctype c) ++ " "

instance Show Treasure where
    show TrueTreasure = "true treasure"
    show FakeTreasure = "fake treasure"

instance Show Health where
    show Healthy = "healthy"
    show Wounded = "wounded"
    show Dead    = "dead"

instance Show Player where
    show p = execWriter $ flip runReaderT p $ do
        tell "Player "
        pos <- view position
        tell $ show pos
        tell ", "
        b <- view pbullets
        tell $ show b
        tell "B"
        tell ", "
        g <- view pgrenades
        tell $ show g
        tell "G"
        h <- view phealth
        when (h /= Healthy) $ do
            tell ", "
            tell $ show h
        f <- view pfell
        when f $
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
          w  = l ^. labWidth
          ws = map (\x -> showH $ l ^?! wallH (Pos x y)) [0..w - 1]

showCellLine :: Labyrinth -> Int -> String
showCellLine l y = concatMap (\x -> showVWall l (Pos x y) ++ showCell l (Pos x y)) [0..w - 1]
                       ++ showVWall l (Pos w y)
                   where w = l ^. labWidth
                         showVWall :: Labyrinth -> Position -> String
                         showVWall l p = showV $ l ^?! wallV p
                         showCell :: Labyrinth -> Position -> String
                         showCell l p = show $ l ^?! cell p

showMap :: Labyrinth -> [String]
showMap l = firstLines ++ [lastLine]
    where h = l ^. labHeight
          showLine l i = [showWallLine l i, showCellLine l i]
          firstLines = concatMap (showLine l) [0..h - 1]
          lastLine = showWallLine l h

showPlayers :: Labyrinth -> [String]
showPlayers l = zipWith showPlayer (l ^. players) [0..]
    where showPlayer p i = show i ++ ": " ++ show p

showCurrentPlayer :: Labyrinth -> [String]
showCurrentPlayer l = ["Current player: " ++ show (l ^. currentTurn)]

showItems :: Labyrinth -> [String]
showItems = concatMap showCellItemsOn . allPosCells
    where showCellItemsOn (p, c) = if itemStr == "" then [] else [showStr]
              where itemStr = showCellItems c
                    showStr = show p ++ ": " ++ itemStr

showCellItems :: Cell -> String
showCellItems c = intercalate ", " $ execWriter $ flip runReaderT c $ do
    b <- view cbullets
    when (b > 0) $ tell [show b ++ "B"]
    g <- view cgrenades
    when (g > 0) $ tell [show g ++ "G"]
    t <- view ctreasures
    tell $ map show t

showStatus :: Labyrinth -> [String]
showStatus l = execWriter $ flip runReaderT l $ do
    pc <- view positionsChosen
    unless pc $ tell ["Positions not chosen"]
    end <- view gameEnded
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
    show (Go d) = "go " ++ show d
    show (Shoot d) = "shoot " ++ show d
    show (Grenade d) = "grenade " ++ show d

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
            let transported = r ^. transportedTo
            when (isJust transported) $ do
                tell ", was transported to "
                tell $ show $ fromJust transported
            let b = r ^. foundBullets
            let g = r ^. foundGrenades
            let t = r ^. foundTreasures
            let found = b > 0 || g > 0 || t > 0
            when found $ do
                tell ", found "
                tell $
                    commaList $
                    map (uncurry pluralize) $
                    filter ((0 <) . fst)
                    [(b, "bullet"), (g, "grenade"), (t, "treasure")]
            return ()
        where pluralize 1 str = "a " ++ str
              pluralize n str = show n ++ " " ++ str ++ "s"
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
        let tr = went ^?! treasureResult
        case tr of
            Just TurnedToAshesR -> tell ", treasure turned to ashes"
            Just TrueTreasureR  -> tell " with a true treasure - victory"
            Nothing             -> return ()
        return ()
    show (GoR InvalidMovement) = "invalid movement"
    show (GoR LostOutside) = "lost outside"

    show (ShootR ShootOK)   = "ok"
    show (ShootR Scream)    = "a scream is heard"
    show (ShootR NoBullets) = "no bullets"
    show (ShootR Forbidden) = "shooting forbidden"

    show (GrenadeR GrenadeOK)  = "ok"
    show (GrenadeR NoGrenades) = "no grenades"

    show Draw = "game ended with a draw"

instance Show StartResult where
    show (StartR pi ct cr) = "player " ++ show pi
                          ++ " started at " ++ show ct ++ show cr

instance Show ChoosePositionResult where
    show ChosenOK          = "position chosen"
    show (AllChosenOK pos) = "game started; " ++ intercalate "; " (map show pos)
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
