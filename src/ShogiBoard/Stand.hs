module ShogiBoard.Stand
  ( Stand
  , pieces
  , take
  , include
  ) where

import Prelude hiding (take)
import ShogiBoard.Piece
import ShogiBoard.Color

-- | 駒台
newtype Stand = Stand [Piece] deriving (Eq, Show)

pieces :: Color -> Stand -> [Piece]
pieces = undefined

take :: Piece -> Color -> Stand -> Maybe (Piece, Stand)
take = undefined

include :: Piece -> Color -> Stand -> Bool
include = undefined
