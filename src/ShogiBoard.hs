module ShogiBoard
  ( ShogiBoard(..)
  , checkmate
  , ShogiBoard.check
  , ShogiBoard.move
  , ShogiBoard.drop
  , ShogiBoard.moves
  , ShogiBoard.drops
  ) where

import Data.List (nub)
import Data.Maybe (maybe)
import Control.Monad (guard)
import ShogiBoard.Board as Board
import ShogiBoard.Stand as Stand
import ShogiBoard.Square
import ShogiBoard.Piece
import ShogiBoard.Color

{--
 F9 F8 F7 F6 F5 F4 F3 F2 F1
                            R1
                            R2
                            R3
                            R4
                            R5
                            R6
 歩 歩 歩 歩 歩 歩 歩 歩 歩 R7
    角                飛    R8
 香 桂 銀 金 王 金 銀 桂 香 R9
--}

-- | 将棋
data ShogiBoard = ShogiBoard { getBoard :: Board, getStand :: Stand } deriving (Eq, Show)

-- | 詰み判定
checkmate :: Color -> ShogiBoard -> Bool
checkmate color shogi = ShogiBoard.check color shogi && null moves' && null drops'
  where
    moves' = do
      (from, _) <- boardPieces color shogi
      ShogiBoard.moves from color shogi
    drops' = do
      piece <- nub $ standPieces color shogi
      ShogiBoard.drops piece shogi

-- | 王手判定
check :: Color -> ShogiBoard -> Bool
check color = Board.check color . getBoard

-- | 将棋盤の駒
boardPieces :: Color -> ShogiBoard -> [(Square, Piece)]
boardPieces color = Board.pieces color . getBoard

-- | 駒台の駒
standPieces :: Color -> ShogiBoard -> [Piece]
standPieces color = Stand.pieces color . getStand

-- | 駒を動かす
move :: MoveFrom -> MoveTo -> Color -> ShogiBoard -> Maybe ShogiBoard
move from to color shogi = do
  board <- Board.move from to color $ getBoard shogi
  return shogi { getBoard = board }

-- | 持ち駒を指す
drop :: Piece -> Square -> Color -> ShogiBoard -> Maybe ShogiBoard
drop piece to color shogi = do
  (piece', stand) <- Stand.take piece     color $ getStand shogi
  board'          <- Board.drop piece' to color $ getBoard shogi
  return shogi { getBoard = board', getStand = stand }

-- | 駒を動かせる升目
moves :: MoveFrom -> Color -> ShogiBoard -> [MoveTo]
moves from color shogi = Board.moves from color $ getBoard shogi

-- | 持ち駒を指せる升目
drops :: Piece -> ShogiBoard -> [Square]
drops piece shogi = do
  guard $ Stand.include piece $ getStand shogi
  Board.drops piece $ getBoard shogi
