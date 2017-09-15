module ShogiBoard where

import Data.List (nub)
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
data Shogi = Shogi { getBoard :: Board, getStand :: Stand } deriving (Eq, Show)

-- | 詰み判定
checkmate :: Color -> Shogi -> Bool
checkmate color shogi = ShogiBoard.check color shogi && null moves' && null drops'
  where
    moves' = do
      (from, _) <- Board.pieces color $ getBoard shogi
      ShogiBoard.moves from color shogi
    drops' = do
      piece <- nub $ Stand.pieces color $ getStand shogi
      ShogiBoard.drops piece color shogi

-- | 王手判定
check :: Color -> Shogi -> Bool
check color = Board.check color . getBoard

-- | 駒を動かす
move :: MoveFrom -> MoveTo -> Color -> Shogi -> Maybe Shogi
move from to color shogi = do
  board <- Board.move from to color $ getBoard shogi
  return shogi { getBoard = board }

-- | 持ち駒を指す
drop :: Piece -> Square -> Color -> Shogi -> Maybe Shogi
drop piece to color shogi = do
  (piece', stand) <- Stand.take piece     color $ getStand shogi
  board'          <- Board.drop piece' to color $ getBoard shogi
  return shogi { getBoard = board', getStand = stand }

-- | 駒を動かせる升目
moves :: MoveFrom -> Color -> Shogi -> [MoveTo]
moves = undefined

-- | 持ち駒を指せる升目
drops :: Piece -> Color -> Shogi -> [Square]
drops = undefined
