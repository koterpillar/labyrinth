{-# Language DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}

module LabyrinthServer.Data where

import Control.Monad.State
import Control.Monad.Reader (ask)

import Data.Acid (Query, Update, makeAcidic)
import Data.DeriveTH
import Data.Derive.Typeable
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable

import Labyrinth hiding (performMove)
import qualified Labyrinth as L

deriveSafeCopy 0 'base ''Direction
deriveSafeCopy 0 'base ''Wall
deriveSafeCopy 0 'base ''CellType
deriveSafeCopy 0 'base ''Cell
deriveSafeCopy 0 'base ''Position
deriveSafeCopy 0 'base ''Treasure
deriveSafeCopy 0 'base ''Player
deriveSafeCopy 0 'base ''Labyrinth

deriveSafeCopy 0 'base ''Action
deriveSafeCopy 0 'base ''MoveDirection
deriveSafeCopy 0 'base ''Move

deriveSafeCopy 0 'base ''CellTypeResult
deriveSafeCopy 0 'base ''GoResult
deriveSafeCopy 0 'base ''GrenadeResult
deriveSafeCopy 0 'base ''ShootResult
deriveSafeCopy 0 'base ''ActionResult
deriveSafeCopy 0 'base ''MoveResult

derive makeTypeable ''Labyrinth
derive makeTypeable ''Move
derive makeTypeable ''MoveResult

performMove :: Move -> Update Labyrinth MoveResult
performMove m = do
    l <- get
    let (mr, nl) = runState (L.performMove m) l
    put nl
    return mr

getLabyrinth :: Query Labyrinth Labyrinth
getLabyrinth = ask

makeAcidic ''Labyrinth ['performMove, 'getLabyrinth]
