module Main where

import Control.Exception (bracket)
import Control.Monad
import Control.Monad.IO.Class

import Data.Acid (AcidState, openLocalState)
import Data.Acid.Advanced (query', update')
import Data.Acid.Local (createCheckpointAndClose)
import Data.List
import qualified Data.Text as T

import Happstack.Server

import System.Random

import Labyrinth hiding (performMove)

import LabyrinthServer.Data

createLabyrinth :: (MonadIO m) => Int -> m Labyrinth
createLabyrinth n = do
    return $ emptyLabyrinth 5 6 $ map (\i -> Pos i i) [0..n - 1]

newId :: (MonadIO m) => m String
newId = do
    gen <- liftIO getStdGen
    return $ T.unpack $ T.unfoldrN 32 (Just . randomR ('a', 'z')) gen

main :: IO ()
main = do
    bracket (openLocalState noGames)
        (createCheckpointAndClose)
        (simpleHTTP nullConf . myApp)

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
    pCount <- lookRead "players"
    gameId <- newId
    lab <- createLabyrinth pCount
    res <- update' acid $ AddGame gameId lab
    if res
        then ok $ toResponse "ok"
        else ok $ toResponse "bad game"

listGames :: AcidState Games -> ServerPart Response
listGames acid = dir "list" $ nullDir >> do
    games <- query' acid $ GameList
    ok $ toResponse $ intercalate ", " $ games

cheat :: AcidState Games -> GameId -> ServerPart Response
cheat acid gameId = dir "cheat" $ nullDir >> do
    l <- query' acid $ ShowLabyrinth gameId
    ok $ toResponse $ show l

showLog :: AcidState Games -> GameId -> ServerPart Response
showLog acid gameId = dir "log" $ nullDir >> do
    l <- query' acid $ GameLog gameId
    let str = intercalate "\n" $ map showMove l
    ok $ toResponse str
    where showMove (m, r) = show m ++ "\n" ++ show r

makeMove :: AcidState Games -> GameId -> ServerPart Response
makeMove acid gameId = dir "move" $ nullDir >> method POST >> do
    decodeBody bodyPolicy
    moveStr <- look "move"
    case parseMove moveStr of
        Left err   -> ok $ toResponse err
        Right move -> do
            res <- update' acid $ PerformMove gameId move
            cp <- query' acid $ CurrentPlayer gameId
            ok $ toResponse $ show res ++ "; current player: " ++ show cp
