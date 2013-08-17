{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Main where

import Control.Applicative
import Control.Concurrent (MVar, newEmptyMVar, withMVar)
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

data LabyrinthServer = LabyrinthServer { lsGames       :: AcidState Games
                                       , lsStatic      :: Static
                                       , lsLock        :: MVar ()
                                       , lsSubscribers :: M.Map GameId (MVar [WS.Sink WS.Hybi00])
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
            lock <- newEmptyMVar
            let server = LabyrinthServer acid static lock M.empty
            app <- toWaiApp server
            let intercept = WaiWS.intercept $ wsHandler server
            let settings = defaultSettings { settingsPort      = port
                                           , settingsHost      = Host ip
                                           , settingsIntercept = intercept
                                           }
            runSettings settings app

wsHandler :: LabyrinthServer -> WS.Request -> WS.WebSockets WS.Hybi00 ()
wsHandler server rq = do
    WS.acceptRequest rq
    WS.getVersion >>= liftIO . putStrLn . ("Client version: " ++)
    sink <- WS.getSink
    WS.sendTextData ("hahaha" :: T.Text)

query :: (QueryEvent event, EventState event ~ Games)
      => event
      -> Handler (EventResult event)
query ev = do
    site <- getYesod
    let acid = lsGames site
    query' acid ev

update :: (UpdateEvent event, EventState event ~ Games)
       => event
       => Handler (EventResult event)
update ev = do
    site <- getYesod
    let acid = lsGames site
    res <- update' acid ev
    liftIO $ withMVar (lsLock site) $ \_ -> do
        putStrLn "event happened"
        let subscribers = lsSubscribers site
        forM_ (M.elems subscribers) $ \subscribedMVar ->
            withMVar subscribedMVar $ \subscribed ->
                forM_ subscribed $ \sink ->
                    WS.sendSink sink $ WS.textData ("event happened" :: T.Text)
        return res

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
getGamesR = do
    games <- query GetGames
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
    lab <- createLabyrinth params
    gameId <- newId
    res <- update $ AddGame gameId lab
    returnJson (if res then "ok" else "bad game" :: String)

getGameR :: GameId -> Handler Value
getGameR gameId = do
    g <- query $ GetGame gameId
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
    case parseMove (T.unpack moveStr) of
        Left err   -> returnJson err
        Right move -> do
            res <- update $ PerformMove gameId playerId move
            returnJson $ show res

deleteDeleteGameR :: GameId -> Handler Value
deleteDeleteGameR gameId = do
    update $ RemoveGame gameId
    returnJson ("ok" :: String)

getExampleMovesR :: Handler Value
getExampleMovesR = returnJson exampleMovesJSON
