{-# Language TemplateHaskell #-}

module Labyrinth.Move where

import Control.Lens hiding (Action)

import Labyrinth.Map

data MoveDirection = Towards Direction | Next
                   deriving (Eq)

type ActionCondition = String

data Action = Go { _amdirection :: MoveDirection }
            | Shoot { _asdirection :: Direction }
            | Grenade { _agdirection :: Direction }
            | Surrender
            | Conditional { _acif   :: ActionCondition
                          , _acthen :: [Action]
                          , _acelse :: [Action]
                          }
            deriving (Eq)

makeLenses ''Action

goTowards :: Direction -> Action
goTowards = Go . Towards

data QueryType = BulletCount
               | GrenadeCount
               | PlayerHealth
               | TreasureCarried
               deriving (Eq)

data Move = Move           { _mactions   :: [Action]    }
          | ChoosePosition { _mcposition :: Position    }
          | ReorderCell    { _mrposition :: Position    }
          | Query          { _mqueries   :: [QueryType] }
          | Say            { _msstext    :: String      }
          deriving (Eq)

makeLenses ''Move

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

data CellEvents = CellEvents { _foundBullets   :: Int
                             , _foundGrenades  :: Int
                             , _foundTreasures :: Int
                             , _transportedTo  :: Maybe CellTypeResult
                             } deriving (Eq)

makeLenses ''CellEvents

noEvents :: CellEvents
noEvents = CellEvents { _foundBullets   = 0
                      , _foundGrenades  = 0
                      , _foundTreasures = 0
                      , _transportedTo  = Nothing
                      }

data GoResult = Went { _onto    :: CellTypeResult
                     , _wevents :: CellEvents
                     }
              | WentOutside { _treasureResult :: Maybe TreasureResult
                            }
              | HitWall { _hitr :: CellEvents
                        }
              | LostOutside
              | InvalidMovement
              deriving (Eq)

makeLenses ''GoResult

data ShootResult = ShootOK
                 | Scream
                 | NoBullets
                 | Forbidden
                 deriving (Eq, Show)

data GrenadeResult = GrenadeOK
                   | NoGrenades
                   deriving (Eq, Show)

data ChoosePositionResult = ChosenOK
                          | ChooseAgain
                          deriving (Eq)

data ReorderCellResult = ReorderOK { _ronto   :: CellTypeResult
                                   , _revents :: CellEvents
                                   }
                       | ReorderForbidden {}
                       deriving (Eq)

makeLenses ''ReorderCellResult

data QueryResult = BulletCountR { _qrbullets :: Int }
                 | GrenadeCountR { _qrgrenades :: Int }
                 | HealthR { _qrhealth :: Health }
                 | TreasureCarriedR { _qrtreasure :: Bool }
                 deriving (Eq)

makeLenses ''QueryResult

data StartResult = StartR { _splayer :: PlayerId
                          , _scell   :: CellTypeResult
                          , _sevents :: CellEvents
                          } deriving (Eq)

makeLenses ''StartResult

data ActionResult = GoR GoResult
                  | ShootR ShootResult
                  | GrenadeR GrenadeResult
                  | Surrendered
                  | WoundedAlert PlayerId Health
                  | ChoosePositionR ChoosePositionResult
                  | ReorderCellR ReorderCellResult
                  | QueryR QueryResult
                  | GameStarted [StartResult]
                  | Draw
                  | WrongTurn
                  | InvalidMove
                  deriving (Eq)

data MoveResult = MoveRes [ActionResult]
                deriving (Eq)
