module Labyrinth.Read (parseMove) where

import Labyrinth.Map
import Labyrinth.Move

import Peeker

import Text.Parsec
import Text.Parsec.String (Parser)

parseMove :: String -> Either String Move
parseMove str = case parse moveParser "" str of
    Right m -> Right m
    Left err -> Left $ show err

stringResult :: String -> a -> Parser a
stringResult s v = do
    string s
    return v

moveParser :: Parser Move
moveParser = do
    spaces
    emptyMove <|> do
        actions <- sepBy1 action (char ',')
        eof
        return $ Move actions

emptyMove :: Parser Move
emptyMove = do
    try $ string "skip"
    return $ Move []

action :: Parser Action
action = do
    spaces
    choice $ map try [ goAction
                     , grenadeAction
                     , shootAction
                     ]

goAction :: Parser Action
goAction = do
    string "go"
    spaces
    choice [ goNext
           , goDirection
           ]
    where goNext = stringResult "next" $ Go Next
          goDirection = do
              d <- direction
              return $ goTowards d

grenadeAction :: Parser Action
grenadeAction = do
    string "grenade"
    spaces
    d <- direction
    return $ Grenade d

shootAction :: Parser Action
shootAction = do
    string "shoot"
    spaces
    d <- direction
    return $ Shoot d

direction :: Parser Direction
direction = choice [ stringResult "left" L
                   , stringResult "right" R
                   , stringResult "up" U
                   , stringResult "down" D
                   ]
