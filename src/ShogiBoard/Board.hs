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

move :: MoveFrom -> MoveTo -> Board -> Maybe Board
move = undefined

drop :: Piece -> Square -> Board -> Maybe Board
drop = undefined

moves :: MoveFrom -> Board -> [MoveTo]
moves = undefined

drops :: Piece -> Board -> [Square]
drops = undefined
