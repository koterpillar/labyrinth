module Main where

import Control.Exception (bracket)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class

import Data.Acid (AcidState, openLocalState)
import Data.Acid.Advanced (query', update')
import Data.Acid.Local (createCheckpointAndClose)
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T

import Happstack.Server hiding (result)

import qualified Text.JSON as J

import System.Environment
import System.Random

import Labyrinth hiding (performMove)

import LabyrinthServer.Data

createLabyrinth :: (MonadIO m) => Int -> Int -> Int -> m Labyrinth
createLabyrinth w h n = do
    gen <- liftIO getStdGen
    let (l, gen') = generateLabyrinth w h n gen
    liftIO $ setStdGen gen'
    return l

newId :: (MonadIO m) => m String
newId = sequence $ replicate 32 $ liftIO $ randomRIO ('a', 'z')

getPort :: IO Int
getPort = do
    env <- getEnvironment
    let envMap = M.fromList env
    let port = M.lookup "PORT" envMap
    let port' = fromMaybe "8000" port
    return $ read port'

main :: IO ()
main = do
    port <- getPort
    let conf = nullConf { port = port }
    bracket (openLocalState noGames)
        createCheckpointAndClose
        (simpleHTTP conf . myApp)

myApp :: AcidState Games -> ServerPart Response
myApp acid = msum (map ($ acid) actions) `mplus` fileServing
    where actions = [ createGame
                    , listGames
                    ]
                    ++ map gameAction gameActions
          gameActions = [ makeMove
                        , showLog
                        , cheat
                        ]

bodyPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

gameAction :: (AcidState Games -> GameId -> ServerPart Response) -> AcidState Games -> ServerPart Response
gameAction act acid = path $ act acid

fileServing :: ServerPart Response
fileServing = serveDirectory DisableBrowsing ["index.html"] "public"

createGame :: AcidState Games -> ServerPart Response
createGame acid = dir "add" $ nullDir >> method POST >> do
    nullDir
    decodeBody bodyPolicy
    lWidth <- lookRead "width"
    lHeight <- lookRead "height"
    pCount <- lookRead "players"
    gameId <- newId
    lab <- createLabyrinth lWidth lHeight pCount
    res <- update' acid $ AddGame gameId lab
    if res
        then ok $ toResponse "ok"
        else ok $ toResponse "bad game"

listGames :: AcidState Games -> ServerPart Response
listGames acid = dir "list" $ nullDir >> do
    games <- query' acid GetGames
    ok $ toResponse $ J.encode $ gameListJSON games

cheat :: AcidState Games -> GameId -> ServerPart Response
cheat acid gameId = dir "cheat" $ nullDir >> do
    g <- query' acid $ GetGame gameId
    ok $ toResponse $ show $ g ^. labyrinth

showLog :: AcidState Games -> GameId -> ServerPart Response
showLog acid gameId = dir "log" $ nullDir >> do
    g <- query' acid $ GetGame gameId
    ok $ toResponse $ J.encode $ gameJSON g

makeMove :: AcidState Games -> GameId -> ServerPart Response
makeMove acid gameId = dir "move" $ nullDir >> method POST >> do
    decodeBody bodyPolicy
    moveStr <- look "move"
    playerId <- lookRead "player"
    case parseMove moveStr of
        Left err   -> ok $ toResponse err
        Right move -> do
            res <- update' acid $ PerformMove gameId playerId move
            ok $ toResponse $ show res
