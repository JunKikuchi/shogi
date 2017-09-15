module ShogiBoard.Board
  ( Board
  , check
  , pieces
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

check :: Color -> Board -> Bool
check = undefined

pieces :: Color -> Board -> [(Square, Piece)]
pieces = undefined

move :: MoveFrom -> MoveTo -> Color -> Board -> Maybe Board
move = undefined

drop :: Piece -> Square -> Color -> Board -> Maybe Board
drop = undefined

moves :: MoveFrom -> Color -> Board -> [MoveTo]
moves = undefined

drops :: Piece -> Color -> Board -> [Square]
drops = undefined
