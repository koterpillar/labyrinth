{-# Language DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}

module LabyrinthServer.Data where

import Control.Lens hiding (Action)
import Control.Monad.State
import Control.Monad.Reader (ask)

import Data.Acid (Query, Update, makeAcidic)
import Data.DeriveTH
import Data.Derive.Typeable
import qualified Data.Map as M
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable

import Text.JSON

import Labyrinth hiding (performMove, currentTurn)
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
deriveSafeCopy 0 'base ''CellEvents
deriveSafeCopy 0 'base ''GoResult
deriveSafeCopy 0 'base ''GrenadeResult
deriveSafeCopy 0 'base ''ShootResult
deriveSafeCopy 0 'base ''ActionResult
deriveSafeCopy 0 'base ''StartResult
deriveSafeCopy 0 'base ''ChoosePositionResult
deriveSafeCopy 0 'base ''ReorderCellResult
deriveSafeCopy 0 'base ''MoveResult

derive makeTypeable ''Labyrinth
derive makeTypeable ''Move
derive makeTypeable ''MoveResult

type GameId = String

data MoveRecord = MoveRecord { _rplayer :: Int
                             , _rmove :: Move
                             , _rresult :: MoveResult
                             }

makeLenses ''MoveRecord

deriveSafeCopy 0 'base ''MoveRecord

derive makeTypeable ''MoveRecord

type MoveLog = [MoveRecord]

logMoveResult :: MoveRecord -> State MoveLog ()
logMoveResult m = modify (\l -> l ++ [m])

data Game = Game { _labyrinth :: Labyrinth
                 , _moves :: MoveLog
                 }

newGame :: Labyrinth -> Game
newGame l = Game l []

makeLenses ''Game

deriveSafeCopy 0 'base ''Game

derive makeTypeable ''Game

data Games = Games { _games :: M.Map GameId Game }

noGames :: Games
noGames = Games M.empty

makeLenses ''Games

game :: GameId -> Simple Traversal Games Game
game gid = games . ix gid

gameList :: Query Games [GameId]
gameList = view games >>= return . M.keys

stateUpdate :: State x y -> Update x y
stateUpdate f = do
    st <- get
    let (r, st') = runState f st
    put st'
    return r

addGame :: GameId -> Labyrinth -> Update Games Bool
addGame gid lab = stateUpdate $ zoom games $ do
    existing <- gets (M.member gid)
    if existing
        then return False
        else do
            modify $ M.insert gid $ newGame lab
            return True

performMove :: GameId -> PlayerId -> Move -> Update Games MoveResult
performMove g p m = stateUpdate $ zoom (singular $ game g) $ do
    r <- zoom labyrinth $ L.performMove p m
    zoom moves $ logMoveResult $ MoveRecord p m r
    return r

currentTurn :: GameId -> Query Games Int
currentTurn g = view $ (singular $ game g) . labyrinth . L.currentTurn

gameLog :: GameId -> Query Games MoveLog
gameLog g = view $ game g . moves

showLabyrinth :: GameId -> Query Games Labyrinth
showLabyrinth g = view (singular $ game g . labyrinth)

deriveSafeCopy 0 'base ''Games

derive makeTypeable ''Games

makeAcidic ''Games [ 'gameList
                   , 'addGame
                   , 'performMove
                   , 'currentTurn
                   , 'gameLog
                   , 'showLabyrinth
                   ]

logJSON :: MoveLog -> JSValue
logJSON g = JSArray $ map moveJSON g
    where moveJSON l = JSObject $ toJSObject [ ("player", jsInt p)
                                             , ("move", jsShow m)
                                             , ("result", jsShow r)
                                             ]
            where p = l ^. rplayer
                  m = l ^. rmove
                  r = l ^. rresult

gameListJSON :: [GameId] -> JSValue
gameListJSON = JSArray . map (JSString . toJSString)

jsInt :: Int -> JSValue
jsInt = JSRational False . fromIntegral

jsShow :: (Show a) => a -> JSValue
jsShow = JSString . toJSString . show
