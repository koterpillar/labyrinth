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
import System.FilePath.Posix
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
newId = replicateM 32 $ liftIO $ randomRIO ('a', 'z')

envVar :: String -> IO (Maybe String)
envVar var = do
    env <- liftM M.fromList getEnvironment
    return $ M.lookup var env

envVarWithDefault :: String -> String -> IO String
envVarWithDefault def var =
    liftM (fromMaybe def) (envVar var)

getDataPath :: IO String
getDataPath = do
    dataDir <- envVarWithDefault "." "OPENSHIFT_DATA_DIR"
    return $ dataDir </> "state"

main :: IO ()
main = do
    ip <- envVarWithDefault "127.0.0.1" "OPENSHIFT_INTERNAL_IP"
    port_ <- liftM read $ envVarWithDefault "8080" "PORT"
    dataPath <- getDataPath
    let conf = nullConf { port = port_ }
    bracket (openLocalState noGames)
        createCheckpointAndClose
        $ \acid -> do
            socket <- bindIPv4 ip (port conf)
            simpleHTTPWithSocket socket conf $ myApp acid

myApp :: AcidState Games -> ServerPart Response
myApp acid = msum (map ($ acid) actions) `mplus` fileServing
    where actions = [ createGame
                    , listGames
                    , listExampleMoves
                    ]
                    ++ map gameAction gameActions
          gameActions = [ makeMove
                        , showLog
                        , deleteGame
                        ]

bodyPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

gameAction :: (AcidState Games -> GameId -> ServerPart Response) -> AcidState Games -> ServerPart Response
gameAction act acid = path $ act acid

fileServing :: ServerPart Response
fileServing = serveDirectory DisableBrowsing ["index.html"] "public"

createGame :: AcidState Games -> ServerPart Response
createGame acid = dir "add" $ nullDir >> method POST >> do
    decodeBody bodyPolicy
    lWidth <- lookRead "width"
    lHeight <- lookRead "height"
    pCount <- lookRead "players"
    gameId <- newId
    lab <- createLabyrinth lWidth lHeight pCount
    res <- update' acid $ AddGame gameId lab
    ok $ toResponse $ if res then "ok" else "bad game"

listGames :: AcidState Games -> ServerPart Response
listGames acid = dir "list" $ nullDir >> do
    games <- query' acid GetGames
    ok $ toResponse $ J.encode $ gameListJSON games

listExampleMoves :: AcidState Games -> ServerPart Response
listExampleMoves _ = dir "examples" $ nullDir >> method GET >> do
    ok $ toResponse $ J.encode $ exampleMovesJSON

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

deleteGame :: AcidState Games -> GameId -> ServerPart Response
deleteGame acid gameId = nullDir >> method DELETE >> do
    update' acid $ RemoveGame gameId
    ok $ toResponse "ok"
