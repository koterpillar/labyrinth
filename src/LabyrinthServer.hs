module LabyrinthServer where

import Control.Exception (bracket)
import Control.Monad (msum)

import Data.Acid (AcidState, openLocalState)
import Data.Acid.Advanced (update')
import Data.Acid.Local (createCheckpointAndClose)

import Happstack.Server

import Labyrinth hiding (performMove)

import LabyrinthServer.Data

createLabyrinth :: IO Labyrinth
createLabyrinth = do
    return $ emptyLabyrinth 5 6 [Pos 0 0, Pos 4 5]

main :: IO ()
main = do
    labyrinth <- createLabyrinth
    bracket (openLocalState labyrinth)
        (createCheckpointAndClose)
        (simpleHTTP nullConf . myApp)

myApp :: AcidState Labyrinth -> ServerPart Response
myApp acid = msum $ map ($ acid) $ [ makeMove
                                   , const fileServing
                                   ]

fileServing :: ServerPart Response
fileServing = serveDirectory DisableBrowsing ["index.html"] "public"

makeMove :: AcidState Labyrinth -> ServerPart Response
makeMove acid = dir "move" $ do
    nullDir
    decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
    moveStr <- look "move"
    case parseMove moveStr of
        Left err   -> ok $ toResponse err
        Right move -> do
            res <- update' acid (PerformMove move)
            ok $ toResponse $ show res
