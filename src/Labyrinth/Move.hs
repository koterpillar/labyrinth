module Labyrinth.Move where

import Labyrinth.Map
import Labyrinth.Show

data Action = Go Direction
            | Shoot Direction
            | Grenade Direction
            deriving (Eq, Show)

data Move = Move [Action]
            deriving (Eq, Show)

data GoResult = WentOnto CellType -- TODO: found
              | HitWall
              deriving (Eq, Show)

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
                  | ActionError String
                  deriving (Eq, Show)

data MoveResult = MoveRes [ActionResult] | MoveError String
     deriving (Eq, Show)
