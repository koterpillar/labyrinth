module Labyrinth.Read (parseMove) where

import Labyrinth.Map
import Labyrinth.Move

import Control.Monad

import Peeker

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as T

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
    emptyMove <|> choosePosition <|> actionsParser

emptyMove :: Parser Move
emptyMove = do
    try $ string "skip"
    return $ Move []

choosePosition :: Parser Move
choosePosition = do
    try $ string "choose"
    spaces
    pos <- positionParser
    return $ ChoosePosition $ pos

positionParser :: Parser Position
positionParser = do
    x <- integer
    spaces
    y <- integer
    return $ Pos x y

integer :: Parser Int
integer = (liftM fromInteger) $ T.integer (T.makeTokenParser emptyDef)

actionsParser :: Parser Move
actionsParser = do
    actions <- sepBy1 action (char ',')
    eof
    return $ Move actions

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
