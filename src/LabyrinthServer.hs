{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Main where

import Control.Applicative
import Control.Concurrent
import Control.Exception (bracket)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class

import Data.Acid ( AcidState
                 , EventResult
                 , EventState
                 , openLocalStateFrom
                 , QueryEvent
                 , UpdateEvent
                 )
import Data.Acid.Advanced (query', update')
import Data.Acid.Local (createCheckpointAndClose)
import Data.Aeson
import qualified Data.ByteString.UTF8 as BSU
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.String as S

import Network.Wai.Handler.Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS

import System.Environment
import System.FilePath.Posix
import System.Random

import Text.Hamlet (hamletFile)
import Text.Julius (juliusFile)
import Text.Lucius (luciusFile)

import Yesod hiding (update)
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

data WatchTarget = GameList | GameLog GameId
                   deriving (Eq, Ord)

type WSType = WS.Hybi00

type WSSink = WS.Sink WSType

data LabyrinthServer = LabyrinthServer { lsGames    :: AcidState Games
                                       , lsStatic   :: Static
                                       , lsWatchers :: MVar (M.Map WatchTarget [WSSink])
                                       }

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

instance Yesod LabyrinthServer where
    defaultLayout = mainLayout

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
    port <- liftM read $ envVarWithDefault "8080" "PORT"
    ip <- envVarWithDefault "127.0.0.1" "OPENSHIFT_INTERNAL_IP"
    static <- static "static"
    bracket
        (openLocalStateFrom dataPath noGames)
        createCheckpointAndClose $
        \acid -> do
            watchers <- newMVar M.empty
            let server = LabyrinthServer acid static watchers
            app <- toWaiApp server
            let intercept = WaiWS.intercept $ wsHandler server
            let settings = defaultSettings { settingsPort      = port
                                           , settingsHost      = Host ip
                                           , settingsIntercept = intercept
                                           }
            runSettings settings app

wsHandler :: LabyrinthServer -> WS.Request -> WS.WebSockets WSType ()
wsHandler site rq = do
    let path = BSU.toString $ WS.requestPath rq
    -- TODO: parse path better
    let watch = if path == "/games" then GameList else GameLog $ drop 6 path
    WS.acceptRequest rq
    sink <- WS.getSink
    addWatcher site watch sink

addWatcher :: (MonadIO m) => LabyrinthServer -> WatchTarget -> WSSink -> m ()
addWatcher site watch sink =
    liftIO $ modifyMVar_ (lsWatchers site) $ \watchers ->
        return $ M.insertWith (++) watch [sink] watchers

query :: (QueryEvent event, EventState event ~ Games)
      => event
      -> Handler (EventResult event)
query ev = do
    site <- getYesod
    let acid = lsGames site
    query' acid ev

update :: (UpdateEvent event, EventState event ~ Games)
       => WatchTarget
       -> event
       -> Handler (EventResult event)
update watch ev = do
    site <- getYesod
    let acid = lsGames site
    res <- update' acid ev
    notifyWatchers site watch
    return res

notifyWatchers :: (MonadIO m) => LabyrinthServer -> WatchTarget -> m ()
notifyWatchers site watch = liftIO $ withMVar (lsWatchers site) $ \watchersMap -> do
    let watchers = fromMaybe [] $ M.lookup watch watchersMap
    value <- watchTargetValue site watch
    forM_ watchers $ \sink ->
        WS.sendSink sink $ WS.textData $ encode value

watchTargetValue :: (MonadIO m) => LabyrinthServer -> WatchTarget -> m Value
watchTargetValue site GameList = do
    let acid = lsGames site
    result <- query' acid GetGames
    return $ toJSON result
watchTargetValue site (GameLog gameId) = do
    let acid = lsGames site
    result <- query' acid $ GetGame gameId
    return $ toJSON result

immediateResponse :: WatchTarget -> Handler Value
immediateResponse target = do
    site <- getYesod
    result <- watchTargetValue site target
    returnJson result

mainLayout :: Widget -> Handler Html
mainLayout widget = do
    p <- widgetToPageContent widget
    giveUrlRenderer $(hamletFile "templates/layout.hamlet")

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/jquery/2.0.3/jquery.min.js"
    addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/handlebars.js/1.0.0/handlebars.min.js"
    toWidget $(juliusFile "templates/index.julius")
    toWidget $(luciusFile "templates/index.lucius")
    $(whamletFile "templates/index.hamlet")

getGamesR :: Handler Value
getGamesR = immediateResponse GameList

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
    lab <- createLabyrinth params
    gameId <- newId
    res <- update GameList $ AddGame gameId lab
    returnJson (if res then "ok" else "bad game" :: String)

getGameR :: GameId -> Handler Value
getGameR gameId = immediateResponse (GameLog gameId)

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
    case parseMove (T.unpack moveStr) of
        Left err   -> returnJson err
        Right move -> do
            res <- update (GameLog gameId) $ PerformMove gameId playerId move
            returnJson $ show res

deleteDeleteGameR :: GameId -> Handler Value
deleteDeleteGameR gameId = do
    update GameList $ RemoveGame gameId
    returnJson ("ok" :: String)

getExampleMovesR :: Handler Value
getExampleMovesR = returnJson exampleMovesJSON
