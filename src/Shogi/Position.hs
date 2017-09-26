module Shogi.Position
  ( Position
  , fromLists
  , toLists
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
import Shogi.Piece (Piece)
import Shogi.Square
import Shogi.Color

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
data Position = Position { getBoard :: Board, getStand :: Stand } deriving (Eq, Show)

-- | 将棋盤作成用リストと駒台作成用リストから将棋作成
fromLists :: ([(Square, Piece)], [(Piece)]) -> Position
fromLists (board, stand) = Position (Board.fromList board) (Stand.fromList stand)

-- | 将棋から将棋盤リストと駒台リストのタプルを作成
toLists :: Position -> ([(Square, Piece)], [(Piece)])
toLists shogi = (Board.toList $ getBoard shogi, Stand.toList $ getStand shogi)

-- | 詰み判定
checkmate :: Color -> Position -> Bool
checkmate color shogi = Shogi.Position.check color shogi && null moves' && null drops'
  where
    moves' = do
      (from, _) <- boardPieces color shogi
      Shogi.Position.moves (from, color) shogi
    drops' = do
      piece <- nub $ standPieces color shogi
      Shogi.Position.drops piece shogi

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
move from@(_, color) to shogi = do
  stand' <- return . maybe stand (flip Stand.put stand) $ Board.lookup (fst to) board
  board' <- Board.move from to board
  guard $ not $ Board.check color board'
  return shogi { getBoard = board', getStand = stand' }
  where
    board = getBoard shogi
    stand = getStand shogi

-- | 持ち駒を指す
drop :: Piece -> Square -> Position -> Maybe Position
drop piece to shogi = do
  stand' <- Stand.take piece    stand
  board' <- Board.drop piece to board
  guard $ not $ Board.check color board'
  return shogi { getBoard = board', getStand = stand' }
  where
    board = getBoard shogi
    stand = getStand shogi
    color = Piece.getColor piece

-- | 駒を動かせる升目
moves :: MoveFrom -> Position -> [MoveTo]
moves from@(_, color) shogi = do
  to     <- Board.moves from board
  board' <- maybeToList $ Board.move from to board
  guard $ not $ Board.check color board'
  return to
  where
    board = getBoard shogi

-- | 持ち駒を指せる升目
drops :: Piece -> Position -> [Square]
drops piece shogi = do
  guard $ Stand.included piece $ getStand shogi
  square <- Board.drops piece board
  board' <- maybeToList $ Board.drop piece square board
  guard $ not $ Board.check color board'
  return square
  where
    board = getBoard shogi
    color = Piece.getColor piece
