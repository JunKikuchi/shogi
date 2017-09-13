module ShogiBoard.Board (Board) where

import qualified Data.Map as Map
import ShogiBoard.Square
import ShogiBoard.Piece

-- | 将棋盤
newtype Board = Board (Map.Map Square Piece) deriving (Eq, Show)
