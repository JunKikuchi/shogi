module ShogiBoard.Board
  ( Board
  , squares
  , move
  , moves
  ) where

import qualified Data.Map as Map
import ShogiBoard.Square
import ShogiBoard.Piece
import ShogiBoard.Color

-- | 将棋盤
newtype Board = Board (Map.Map Square Piece) deriving (Eq, Show)

toList :: Board -> [(Square, Piece)]
toList = undefined

squares :: Color -> Board -> [Square]
squares = undefined

move :: MoveFrom -> MoveTo -> Board -> Maybe Board
move = undefined

moves :: MoveFrom -> Board -> [MoveTo]
moves = undefined
