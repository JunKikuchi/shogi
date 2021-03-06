module Shogi.Board
  ( Board
  , fromList
  , toList
  , toMap
  , check
  , pieces
  , move
  , drop
  , moves
  , drops
  , lookup
  ) where

import           Control.Monad (guard)
import qualified Data.Map      as Map
import           Data.Maybe    (isNothing, maybe, maybeToList)
import           Prelude       hiding (drop, lookup)
import           Shogi.Color
import           Shogi.Piece   (Piece, pieceColor, piecePromotion, pieceType)
import qualified Shogi.Piece   as Piece
import           Shogi.Square

-- | 将棋盤
newtype Board = Board { unBoard :: Map.Map Square Piece } deriving (Eq, Show)

-- | 升目と駒のリストから将棋盤作成
fromList :: [(Square, Piece)] -> Board
fromList = Board . Map.fromList

-- | 将棋盤の升目と駒のリスト
toList :: Board -> [(Square, Piece)]
toList (Board board) = Map.toList board

-- | 将棋盤の升目をキーに駒を値にしたマップ
toMap :: Board -> Map.Map Square Piece
toMap = unBoard

-- | 王手判定
check :: Color -> Board -> Bool
check color board = maybe False (`elem` moves') $ kingSquare color board
  where
    moves' = do
      (from, _) <- pieces turnedColor board
      (to,   _) <- Shogi.Board.moves (from, turnedColor) board
      return to
    turnedColor = turn color

-- | 王の升目
kingSquare :: Color -> Board -> Maybe Square
kingSquare color board = if null kings then Nothing else Just $ fst $ head kings
  where
    kings = filter (\(_, piece) -> piece == Piece.king color) $ pieces color board

-- | 手番の駒リスト
pieces :: Color -> Board -> [(Square, Piece)]
pieces color = filter (\(_, piece) -> color == pieceColor piece) . toList

-- | 駒を動かす
move :: MoveFrom -> MoveTo -> Board -> Maybe Board
move from@(from', color) moveTo@(to, promoted) board = do
  piece <- lookup from' board
  guard $ pieceColor piece == color
  guard $ elem moveTo $ Shogi.Board.moves from board
  let deletedBoard = Map.delete from' (unBoard board)
  return board { unBoard = Map.insert to piece { piecePromotion = piecePromotion piece || promoted } deletedBoard }

-- | 持ち駒を指す
drop :: Piece -> Square -> Board -> Maybe Board
drop piece square board = if drops' then Just drop' else Nothing
  where
    drops' = elem square $ Shogi.Board.drops piece board
    drop'  = board { unBoard = Map.insert square piece $ unBoard board }

-- | 駒を動かせる升目リスト
moves :: MoveFrom -> Board -> [MoveTo]
moves from@(from', color) board = do
  piece <- maybeToList $ lookup from' board
  guard $ pieceColor piece == color
  squares <- Piece.moves piece from'
  moves' squares piece board
  where
    moves' [] _ _ = []
    moves' (square:squares) piece board =
      case lookup square board of
        (Just piece') -> if pieceColor piece' == pieceColor piece
                         then []
                         else moveTo'
        Nothing       -> moveTo' ++ moves' squares piece board
      where
        moveTo' = [(square, promotion) | promotion <- Piece.promotions piece from' square]

-- | 持ち駒を指せる升目リスト
drops :: Piece -> Board -> [Square]
drops piece board
  | pieceType piece == Piece.Pawn = filterPawnFiles squares
  | otherwise                         = squares
  where
    filterPawnFiles = filter (\(file, _) -> notElem file pawnFiles)
    pawnFiles       = map (fst . fst) $ filter (\(_, piece) -> pieceType piece == Piece.Pawn && not (piecePromotion piece)) $ pieces (pieceColor piece) board
    squares         = filter (\square -> isNothing (lookup square board)) $ Piece.drops piece

-- | 升目の駒
lookup :: Square -> Board -> Maybe Piece
lookup square (Board board) = Map.lookup square board
