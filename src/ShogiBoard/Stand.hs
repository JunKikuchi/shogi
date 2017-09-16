module ShogiBoard.Stand
  ( Stand
  , fromList
  , pieces
  , take
  , include
  ) where

import Prelude hiding (take)
import ShogiBoard.Piece
import ShogiBoard.Color

-- | 駒台
newtype Stand = Stand [Piece] deriving (Eq, Show)

fromList :: [Piece] -> Stand
fromList = undefined

pieces :: Color -> Stand -> [Piece]
pieces = undefined

take :: Piece -> Color -> Stand -> Maybe (Piece, Stand)
take = undefined

include :: Piece -> Color -> Stand -> Bool
include = undefined
