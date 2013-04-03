module Labyrinth.Common where

import Control.Monad.State

import Labyrinth.Map

type LabState m a = StateT Labyrinth m a
