module Shogi.States (States) where

import Shogi.State

newtype States = States [State] deriving (Eq, Show)
