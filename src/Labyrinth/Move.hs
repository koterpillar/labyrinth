{-# Language TemplateHaskell #-}

module Labyrinth.Move where

import Labyrinth.Map

import Peeker

data MoveDirection = Towards Direction | Next
                   deriving (Eq)

data Action = Go MoveDirection
            | Shoot Direction
            | Grenade Direction
            deriving (Eq)

goTowards :: Direction -> Action
goTowards = Go . Towards

data Move = Move [Action]
            deriving (Eq)

data CellTypeResult = LandR
                    | ArmoryR
                    | HospitalR
                    | PitR
                    | RiverR
                    | RiverDeltaR
                    deriving (Eq)

ctResult :: CellType -> CellTypeResult
ctResult Land       = LandR
ctResult Armory     = ArmoryR
ctResult Hospital   = HospitalR
ctResult (Pit _)    = PitR
ctResult (River _)  = RiverR
ctResult RiverDelta = RiverDeltaR

data TreasureResult = TurnedToAshesR
                    | TrueTreasureR
                    deriving (Eq)

data GoResult = Went { onto_           :: CellTypeResult
                     , foundBullets_   :: Int
                     , foundGrenades_  :: Int
                     , foundTreasures_ :: Int
                     , transportedTo_  :: Maybe CellTypeResult
                     }
              | WentOutside { treasureResult_ :: Maybe TreasureResult
                            }
              | HitWall {}
              deriving (Eq)

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
                  deriving (Eq)

data MoveResult = MoveRes [ActionResult]
                | WrongTurn
                | InvalidMove
                deriving (Eq)
