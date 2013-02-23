{-# Language DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}

module LabyrinthServer.Data where

import Control.Monad.State
import Control.Monad.Reader (ask)

import Data.Acid (Query, Update, makeAcidic)
import Data.DeriveTH
import Data.Derive.Typeable
import Data.Map
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable

import Peeker

import Labyrinth hiding (performMove, currentPlayer)
import qualified Labyrinth as L

deriveSafeCopy 0 'base ''Direction
deriveSafeCopy 0 'base ''Wall
deriveSafeCopy 0 'base ''CellType
deriveSafeCopy 0 'base ''Cell
deriveSafeCopy 0 'base ''Position
deriveSafeCopy 0 'base ''Treasure
deriveSafeCopy 0 'base ''Health
deriveSafeCopy 0 'base ''Player
deriveSafeCopy 0 'base ''Labyrinth

deriveSafeCopy 0 'base ''Action
deriveSafeCopy 0 'base ''MoveDirection
deriveSafeCopy 0 'base ''Move

deriveSafeCopy 0 'base ''CellTypeResult
deriveSafeCopy 0 'base ''TreasureResult
deriveSafeCopy 0 'base ''CellResult
deriveSafeCopy 0 'base ''GoResult
deriveSafeCopy 0 'base ''GrenadeResult
deriveSafeCopy 0 'base ''ShootResult
deriveSafeCopy 0 'base ''ActionResult
deriveSafeCopy 0 'base ''ChoosePositionResult
deriveSafeCopy 0 'base ''ReorderCellResult
deriveSafeCopy 0 'base ''MoveResult

derive makeTypeable ''Labyrinth
derive makeTypeable ''Move
derive makeTypeable ''MoveResult

type GameId = String

data MoveRecord = MoveRecord { rplayer_ :: Int
                             , rmove_ :: Move
                             , rresult_ :: MoveResult
                             }

derivePeek ''MoveRecord

deriveSafeCopy 0 'base ''MoveRecord

derive makeTypeable ''MoveRecord

type MoveLog = [MoveRecord]

logMoveResult :: MoveRecord -> State MoveLog ()
logMoveResult m = modify (\l -> l ++ [m])

data Game = Game { labyrinth_ :: Labyrinth
                 , moves_ :: MoveLog
                 }

newGame :: Labyrinth -> Game
newGame l = Game l []

derivePeek ''Game

deriveSafeCopy 0 'base ''Game

derive makeTypeable ''Game

data Games = Games { games_ :: Map GameId Game }

noGames :: Games
noGames = Games empty

derivePeek ''Games

game :: GameId -> Peek Games Game
game id = games ~> mapP id

gameList :: Query Games [GameId]
gameList = askS games >>= return . keys

addGame :: GameId -> Labyrinth -> Update Games Bool
addGame id lab = stateS games $ do
    existing <- gets (member id)
    if existing
        then return False
        else do
            modify $ insert id $ newGame lab
            return True

performMove :: GameId -> PlayerId -> Move -> Update Games MoveResult
performMove g p m = stateS (game g) $ do
    r <- stateS labyrinth $ L.performMove p m
    stateS moves $ logMoveResult $ MoveRecord p m r
    return r

currentPlayer :: GameId -> Query Games Int
currentPlayer g = askS $ game g ~> labyrinth ~> L.currentPlayer

gameLog :: GameId -> Query Games MoveLog
gameLog g = askS $ game g ~> moves

showLabyrinth :: GameId -> Query Games Labyrinth
showLabyrinth g = askS (game g ~> labyrinth)

deriveSafeCopy 0 'base ''Games

derive makeTypeable ''Games

makeAcidic ''Games [ 'gameList
                   , 'addGame
                   , 'performMove
                   , 'currentPlayer
                   , 'gameLog
                   , 'showLabyrinth
                   ]
