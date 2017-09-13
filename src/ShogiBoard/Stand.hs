module ShogiBoard.Stand (Stand) where

import ShogiBoard.Piece

-- | 駒台
newtype Stand = Stand [Piece] deriving (Eq, Show)
