module ShogiBoard.Stand
  ( Stand
  , fromList
  , toList
  , pieces
  , take
  , included
  ) where

import Prelude hiding (take)
import ShogiBoard.Piece
import ShogiBoard.Color

-- | 駒台
newtype Stand = Stand [Piece] deriving (Eq, Show)

fromList :: [Piece] -> Stand
fromList = Stand

toList :: Stand -> [Piece]
toList (Stand stand) = stand

pieces :: Color -> Stand -> [Piece]
pieces color (Stand stand) = filter (\piece -> getColor piece == color) stand

take :: Piece -> Color -> Stand -> Maybe (Piece, Stand)
take = undefined

included :: Piece -> Stand -> Bool
included piece (Stand stand) = elem piece stand
