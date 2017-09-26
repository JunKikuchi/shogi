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
toLists position = (Board.toList $ getBoard position, Stand.toList $ getStand position)

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
  guard $ not $ dropPawnMate piece to position
  return position { getBoard = board', getStand = stand' }
  where
    board = getBoard position
    stand = getStand position
    color = Piece.getColor piece

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
  guard $ not $ dropPawnMate piece square position
  return square
  where
    board = getBoard position
    color = Piece.getColor piece

-- | 打ち歩詰め判定
dropPawnMate :: Piece -> Square -> Position -> Bool
dropPawnMate piece square position
  | Piece.getType piece /= Piece.Pawn = False
  | otherwise = maybe False checkmate' $ do
    board <- Board.drop piece square $ getBoard position
    return position { getBoard = board }
    where
      checkmate' = checkmate $ turn $ Piece.getColor piece
