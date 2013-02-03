{-# Language TemplateHaskell #-}

module Labyrinth.Move where

import Labyrinth.Map
import Labyrinth.Show

import Peeker

data MoveDirection = Towards Direction | Next
                   deriving (Eq, Show)

data Action = Go MoveDirection
            | Shoot Direction
            | Grenade Direction
            deriving (Eq, Show)

goTowards :: Direction -> Action
goTowards = Go . Towards

data Move = Move [Action]
            deriving (Eq, Show)

data CellTypeResult = LandR
                    | ArmoryR
                    | HospitalR
                    | PitR
                    | RiverR
                    | RiverDeltaR
                    deriving (Eq, Show)

ctResult :: CellType -> CellTypeResult
ctResult Land       = LandR
ctResult Armory     = ArmoryR
ctResult Hospital   = HospitalR
ctResult (Pit _)    = PitR
ctResult (River _)  = RiverR
ctResult RiverDelta = RiverDeltaR

data GoResult = Went { onto_           :: CellTypeResult
                     , foundBullets_   :: Int
                     , foundGrenades_  :: Int
                     , foundTreasures_ :: Int
                     , transportedTo_  :: Maybe CellTypeResult
                     }
              | HitWall {}
              deriving (Eq, Show)

derivePeek ''GoResult

data ShootResult = ShootOK
                 | Scream
                 | NoBullets
                 | Forbidden
                 deriving (Eq, Show)

data GrenadeResult = GrenadeOK
                   | NoGrenades
                   deriving (Eq, Show)

data ActionResult = GoR GoResult
                  | ShootR ShootResult
                  | GrenadeR GrenadeResult
                  deriving (Eq, Show)

data MoveResult = MoveRes [ActionResult]
     deriving (Eq, Show)
