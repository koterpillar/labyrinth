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
import qualified Data.String as S

import System.Environment
import System.FilePath.Posix
import System.Random

import Text.Julius (juliusFile)
import Text.Lucius (luciusFile)

import Yesod
import Yesod.Static

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

data LabyrinthServer = LabyrinthServer { lsGames  :: AcidState Games
                                       , lsStatic :: Static
                                       }

instance Yesod LabyrinthServer

staticFiles "static"

mkYesod "LabyrinthServer" [parseRoutes|
/                    HomeR          GET
/games               GamesR         GET
/game                NewGameR       POST
/game/#GameId        GameR          GET
/game/#GameId/move   MakeMoveR      POST
/game/#GameId/delete DeleteGameR    DELETE
/examples            ExampleMovesR  GET
/static              StaticR        Static lsStatic
|]

instance RenderMessage LabyrinthServer FormMessage where
    renderMessage _ _ = defaultFormMessage

postForm :: (Html -> MForm Handler (FormResult a, Widget))
         -> (a -> Handler Value)
         -> Handler Value
postForm form handler = do
    ((result, _), _) <- runFormPostNoToken form
    case result of
        FormSuccess value -> handler value
        FormFailure errors -> returnJson errors

main :: IO ()
main = do
    dataPath <- getDataPath
    static <- static "static"
    bracket (openLocalState noGames)
        createCheckpointAndClose
        $ \acid -> warpEnv (LabyrinthServer acid static)

getAcid = liftM lsGames getYesod

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    toWidget $(juliusFile "templates/index.julius")
    addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/jquery/2.0.3/jquery.min.js"
    addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/handlebars.js/1.0.0/handlebars.min.js"
    toWidget $(luciusFile "templates/index.lucius")
    setTitle "Labyrinth"
    $(whamletFile "templates/index.hamlet")

getGamesR :: Handler Value
getGamesR = do
    acid <- getAcid
    games <- query' acid GetGames
    returnJson games

named :: T.Text -> FieldSettings LabyrinthServer
named name = FieldSettings "" Nothing Nothing (Just name) []

newGameForm :: Html
            -> MForm Handler (FormResult LabyrinthParams, Widget)
newGameForm = renderDivs $ LabyrinthParams
    <$> areq intField (named "width") Nothing
    <*> areq intField (named "height") Nothing
    <*> areq intField (named "players") Nothing

postNewGameR :: Handler Value
postNewGameR = postForm newGameForm $ \params -> do
    acid <- getAcid
    lab <- createLabyrinth params
    gameId <- newId
    res <- update' acid $ AddGame gameId lab
    returnJson (if res then "ok" else "bad game" :: String)

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
    <$> areq intField (named "player") Nothing
    <*> areq textField (named "move") Nothing

postMakeMoveR :: GameId -> Handler Value
postMakeMoveR gameId = postForm makeMoveForm $ \playerMove -> do
    let PlayerMove playerId moveStr = playerMove
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
getExampleMovesR = returnJson exampleMovesJSON
