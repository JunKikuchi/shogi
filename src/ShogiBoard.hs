module ShogiBoard where

import Data.Maybe (catMaybes)
import Data.List (nub)
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
checkmate color shogi = ShogiBoard.check color shogi && check_moves && check_drops
  where
    check_moves = check' boards
      where
        boards = catMaybes [Board.move from to color board | (from, _) <- pieces, to <- Board.moves from color board]
        pieces = Board.pieces color board
    check_drops = check' drops
      where
        drops  = catMaybes [Board.drop piece to color board | piece <- pieces, to <- Board.drops piece color board]
        pieces = nub $ Stand.pieces color stand
        stand  = getStand shogi
    check' = all id . map (\board' -> ShogiBoard.check color shogi { getBoard = board' })
    board  = getBoard shogi

-- | 王手判定
check :: Color -> Shogi -> Bool
check color = Board.check color . getBoard

-- | 駒を動かす
move :: MoveFrom -> MoveTo -> Color -> Shogi -> Maybe Shogi
move = undefined

-- | 持ち駒を指す
drop :: Piece -> MoveTo -> Color -> Shogi -> Maybe Shogi
drop = undefined

-- | 駒を動かせる升目
moves :: MoveFrom -> Color -> Shogi -> [MoveTo]
moves = undefined

-- | 持ち駒を指せる升目
drops :: Piece -> Color -> Shogi -> [Square]
drops = undefined
