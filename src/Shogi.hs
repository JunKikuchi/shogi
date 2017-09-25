module Shogi
  ( Shogi
  , getColor
  , getClock
  , getStates
  , getState
  , Move
  , shogi
  , move
  , checkmate
  , check
  ) where

import Shogi.Color
import Shogi.Clock
import Shogi.State
import Shogi.States
import Shogi.Piece (Piece)
import ShogiBoard.Square

data Shogi = Shogi
           { getColor  :: Color
           , getClock  :: Clock
           , getStates :: States
           } deriving (Eq, Show)

data Move = Move MoveFrom MoveTo Sec
          | Drop Piece    MoveTo Sec
          | Resign               Sec
          | Clock                Sec
          deriving (Eq, Show)

shogi :: Color -> Clock -> States -> Shogi
shogi = undefined

getState :: Shogi -> State
getState = undefined

checkmate :: Color -> Shogi -> Bool
checkmate = undefined

check :: Color -> Shogi -> Bool
check = undefined

move :: Move -> Shogi -> Maybe Shogi
move = undefined
