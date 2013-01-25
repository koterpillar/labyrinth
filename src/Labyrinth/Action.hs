module Labyrinth.Action where

import Control.Monad.State

import Labyrinth.Map
import Labyrinth.Move

performMove :: Move -> State Labyrinth MoveResult
performMove = undefined
