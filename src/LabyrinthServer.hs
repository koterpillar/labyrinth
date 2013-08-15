{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Main where

import Control.Applicative
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

import qualified Text.JSON as J

import System.Environment
import System.FilePath.Posix
import System.Random

import Yesod

import Labyrinth hiding (performMove)

import LabyrinthServer.Data

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

data LabyrinthServer = LabyrinthServer { lsGames :: AcidState Games
                                       }

instance Yesod LabyrinthServer

mkYesod "LabyrinthServer" [parseRoutes|
/games               GamesR         GET
/game                NewGameR       POST
/game/#GameId        GameR          GET
/game/#GameId/move   MakeMoveR      POST
/game/#GameId/delete DeleteGameR    DELETE
/examples            ExampleMovesR  GET
|]

main :: IO ()
main = do
    dataPath <- getDataPath
    bracket (openLocalState noGames)
        createCheckpointAndClose
        $ \acid -> warpEnv (LabyrinthServer acid)

getAcid = liftM lsGames getYesod

getGamesR :: Handler Value
getGamesR = do
    acid <- getAcid
    games <- query' acid GetGames
    returnJson games

instance RenderMessage LabyrinthServer FormMessage where
    renderMessage _ _ = defaultFormMessage

newGameForm :: Html
            -> MForm Handler (FormResult LabyrinthParams, Widget)
newGameForm = renderDivs $ LabyrinthParams
    <$> areq intField "Width" Nothing
    <*> areq intField "Height" Nothing
    <*> areq intField "Players" Nothing

postNewGameR :: Handler Value
postNewGameR = do
    ((result, _), _) <- runFormPost newGameForm
    case result of
        FormSuccess params -> do
            acid <- getAcid
            lab <- createLabyrinth params
            gameId <- newId
            res <- update' acid $ AddGame gameId lab
            returnJson $ (if res then "ok" else "bad game" :: String)

getGameR :: GameId -> Handler Value
getGameR gameId = do
    acid <- getAcid
    g <- query' acid $ GetGame gameId
    returnJson g

data PlayerMove = PlayerMove { pmplayer :: PlayerId
                             , pmmove   :: T.Text
                             }

makeMoveForm :: Html
             -> MForm Handler (FormResult PlayerMove, Widget)
makeMoveForm = renderDivs $ PlayerMove
    <$> areq intField "player" Nothing
    <*> areq textField "move" Nothing

postMakeMoveR :: GameId -> Handler Value
postMakeMoveR gameId = do
    ((result, _), _) <- runFormPost makeMoveForm
    case result of
        FormSuccess (PlayerMove playerId moveStr) -> do
            acid <- getAcid
            case parseMove (T.unpack moveStr) of
                Left err   -> returnJson err
                Right move -> do
                    res <- update' acid $ PerformMove gameId playerId move
                    returnJson $ show res

deleteDeleteGameR :: GameId -> Handler Value
deleteDeleteGameR gameId = do
    acid <- getAcid
    update' acid $ RemoveGame gameId
    returnJson ("ok" :: String)

getExampleMovesR :: Handler Value
getExampleMovesR = do
    returnJson $ J.encode $ exampleMovesJSON
