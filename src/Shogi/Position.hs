module Shogi.Position
  ( Position
  , fromLists
  , toLists
  , hirate
  , checkmate
  , check
  , move
  , drop
  , moves
  , drops
  ) where

import Prelude hiding (drop)
import Data.List (nub)
import Data.Maybe (maybe, maybeToList)
import Control.Monad (guard)
import qualified Shogi.Board as Board
import qualified Shogi.Stand as Stand
import qualified Shogi.Piece as Piece
import Shogi.Board (Board)
import Shogi.Stand (Stand)
import Shogi.Piece (Piece, pieceType, pieceColor)
import Shogi.Square
import Shogi.Color

{--
 F9 F8 F7 F6 F5 F4 F3 F2 F1
V香V桂V銀V金V王V金V銀V桂V香 R1
   V飛               V角    R2
V歩V歩V歩V歩V歩V歩V歩V歩V歩 R3
                            R4
                            R5
                            R6
 歩 歩 歩 歩 歩 歩 歩 歩 歩 R7
    角                飛    R8
 香 桂 銀 金 王 金 銀 桂 香 R9
--}

-- | 局面
data Position = Position { getBoard :: Board, getStand :: Stand } deriving (Eq, Show)

-- | 将棋盤作成用リストと駒台作成用リストから局面作成
fromLists :: ([(Square, Piece)], [(Piece)]) -> Position
fromLists (board, stand) = Position (Board.fromList board) (Stand.fromList stand)

-- | 局面から将棋盤リストと駒台リストのタプルを作成
toLists :: Position -> ([(Square, Piece)], [(Piece)])
toLists position = (Board.toList $ getBoard position, Stand.toList $ getStand position)

-- | 平手の局面を作成
hirate :: Position
hirate = fromLists (board, [])
  where
    board = [ ((F9, R1), Piece.lance  False White)
            , ((F8, R1), Piece.knight False White)
            , ((F7, R1), Piece.silver False White)
            , ((F6, R1), Piece.gold         White)
            , ((F5, R1), Piece.king         White)
            , ((F4, R1), Piece.gold         White)
            , ((F3, R1), Piece.silver False White)
            , ((F2, R1), Piece.knight False White)
            , ((F1, R1), Piece.lance  False White)
            , ((F8, R2), Piece.rook   False White)
            , ((F2, R2), Piece.bishop False White)
            , ((F9, R3), Piece.pawn   False White)
            , ((F8, R3), Piece.pawn   False White)
            , ((F7, R3), Piece.pawn   False White)
            , ((F6, R3), Piece.pawn   False White)
            , ((F5, R3), Piece.pawn   False White)
            , ((F4, R3), Piece.pawn   False White)
            , ((F3, R3), Piece.pawn   False White)
            , ((F2, R3), Piece.pawn   False White)
            , ((F1, R3), Piece.pawn   False White)
            , ((F9, R7), Piece.pawn   False Black)
            , ((F8, R7), Piece.pawn   False Black)
            , ((F7, R7), Piece.pawn   False Black)
            , ((F6, R7), Piece.pawn   False Black)
            , ((F5, R7), Piece.pawn   False Black)
            , ((F4, R7), Piece.pawn   False Black)
            , ((F3, R7), Piece.pawn   False Black)
            , ((F2, R7), Piece.pawn   False Black)
            , ((F1, R7), Piece.pawn   False Black)
            , ((F8, R8), Piece.bishop False Black)
            , ((F2, R8), Piece.rook   False Black)
            , ((F9, R9), Piece.lance  False Black)
            , ((F8, R9), Piece.knight False Black)
            , ((F7, R9), Piece.silver False Black)
            , ((F6, R9), Piece.gold         Black)
            , ((F5, R9), Piece.king         Black)
            , ((F4, R9), Piece.gold         Black)
            , ((F3, R9), Piece.silver False Black)
            , ((F2, R9), Piece.knight False Black)
            , ((F1, R9), Piece.lance  False Black)
            ]

-- | 詰み判定
checkmate :: Color -> Position -> Bool
checkmate color position = Shogi.Position.check color position && null moves' && null drops'
  where
    moves' = do
      (from, _) <- boardPieces color position
      Shogi.Position.moves (from, color) position
    drops' = do
      piece <- nub $ standPieces color position
      Shogi.Position.drops piece position

-- | 王手判定
check :: Color -> Position -> Bool
check color = Board.check color . getBoard

-- | 将棋盤の駒
boardPieces :: Color -> Position -> [(Square, Piece)]
boardPieces color = Board.pieces color . getBoard

-- | 駒台の駒
standPieces :: Color -> Position -> [Piece]
standPieces color = Stand.pieces color . getStand

-- | 駒を動かす
move :: MoveFrom -> MoveTo -> Position -> Maybe Position
move from@(_, color) to position = do
  stand' <- return . maybe stand (flip Stand.put stand) $ Board.lookup (fst to) board
  board' <- Board.move from to board
  guard $ not $ Board.check color board'
  return position { getBoard = board', getStand = stand' }
  where
    board = getBoard position
    stand = getStand position

-- | 持ち駒を指す
drop :: Piece -> Square -> Position -> Maybe Position
drop piece to position = do
  stand' <- Stand.take piece    stand
  board' <- Board.drop piece to board
  guard $ not $ Board.check color board'
  let position' = position { getBoard = board', getStand = stand' }
  guard $ not $ drawPawnMate piece position'
  return position'
  where
    board = getBoard position
    stand = getStand position
    color = pieceColor piece

-- | 駒を動かせる升目
moves :: MoveFrom -> Position -> [MoveTo]
moves from@(_, color) position = do
  to     <- Board.moves from board
  board' <- maybeToList $ Board.move from to board
  guard $ not $ Board.check color board'
  return to
  where
    board = getBoard position

-- | 持ち駒を指せる升目
drops :: Piece -> Position -> [Square]
drops piece position = do
  guard $ Stand.included piece $ getStand position
  square <- Board.drops piece board
  board' <- maybeToList $ Board.drop piece square board
  guard $ not $ Board.check color board'
  guard $ not $ drawPawnMate piece position { getBoard = board' }
  return square
  where
    board = getBoard position
    color = pieceColor piece

-- | 打ち歩詰め判定
drawPawnMate :: Piece -> Position -> Bool
drawPawnMate piece position = pieceType piece == Piece.Pawn && checkmate turnColor position
  where
    turnColor = turn $ pieceColor piece
