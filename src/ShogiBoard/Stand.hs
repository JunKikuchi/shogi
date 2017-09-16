module ShogiBoard.Stand
  ( Stand
  , fromList
  , toList
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

toList :: Stand -> [Piece]
toList = undefined

pieces :: Color -> Stand -> [Piece]
pieces = undefined

take :: Piece -> Color -> Stand -> Maybe (Piece, Stand)
take = undefined

include :: Piece -> Stand -> Bool
include = undefined
