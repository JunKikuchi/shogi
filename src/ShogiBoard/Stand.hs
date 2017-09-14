module ShogiBoard.Stand
  ( Stand
  , pieces
  ) where

import ShogiBoard.Piece
import ShogiBoard.Color

-- | 駒台
newtype Stand = Stand [Piece] deriving (Eq, Show)

pieces :: Color -> Stand -> [Piece]
pieces = undefined
