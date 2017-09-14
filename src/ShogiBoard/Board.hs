module ShogiBoard.Board
  ( Board
  , squares
  , move
  , drop
  , moves
  , drops
  ) where

import Prelude hiding (drop)
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

drop :: Piece -> Square -> Board -> Maybe Board
drop = undefined

moves :: MoveFrom -> Board -> [MoveTo]
moves = undefined

drops :: Color -> Board -> [Square]
drops = undefined
