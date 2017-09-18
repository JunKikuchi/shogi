module ShogiBoard.Board
  ( Board
  , fromList
  , toList
  , check
  , pieces
  , move
  , drop
  , ShogiBoard.Board.moves
  , ShogiBoard.Board.drops
  ) where

import Prelude hiding (drop, lookup)
import Data.Maybe (maybe, maybeToList)
import qualified Data.Map as Map
import Control.Monad (guard)
import ShogiBoard.Square
import ShogiBoard.Piece as Piece
import ShogiBoard.Color

-- | 将棋盤
newtype Board = Board { getBoard :: (Map.Map Square Piece) } deriving (Eq, Show)

-- | 升目と駒のリストから将棋盤作成
fromList :: [(Square, Piece)] -> Board
fromList = Board . Map.fromList

-- | 将棋盤の升目と駒のリスト
toList :: Board -> [(Square, Piece)]
toList (Board board) = Map.toList board

-- | 王手判定
check :: Color -> Board -> Bool
check color board = maybe False (flip elem moves') $ kingSquare color board
  where
    moves' = do
      (from, _) <- pieces turnedColor board
      (to,   _) <- ShogiBoard.Board.moves from turnedColor board
      return to
    turnedColor = turn color

-- | 王の升目
kingSquare :: Color -> Board -> Maybe Square
kingSquare color board = if null kings then Nothing else Just $ fst $ head kings
  where
    kings = filter (\(_, piece) -> piece == king color) $ pieces color board

-- | 手番の駒リスト
pieces :: Color -> Board -> [(Square, Piece)]
pieces color = filter (\(_, piece) -> color == getColor piece) . toList

-- | 駒を動かす
move :: MoveFrom -> MoveTo -> Color -> Board -> Maybe Board
move from moveTo@(to, promoted) color board = do
  piece <- lookup from board
  guard $ getColor piece == color
  guard $ elem moveTo $ ShogiBoard.Board.moves from color board
  let deletedBoard = Map.delete from (getBoard board)
  return board { getBoard = Map.alter (const $ Just piece { getPromoted = promoted }) to deletedBoard }

-- | 持ち駒を指す
drop :: Piece -> Square -> Color -> Board -> Maybe Board
drop = undefined

-- | 駒を動かせる升目リスト
moves :: MoveFrom -> Color -> Board -> [MoveTo]
moves from color board = do
  piece <- maybeToList $ lookup from board
  guard $ getColor piece == color
  squares <- Piece.moves piece from
  moves' squares piece board
  where
    moves' [] _ _ = []
    moves' (square:squares) piece board = do
      case lookup square board of
        (Just piece') -> if getColor piece' == getColor piece
                         then []
                         else moveTo'
        Nothing       -> moveTo' ++ moves' squares piece board
      where
        moveTo' = [(square, promotion) | promotion <- promotions piece square]

-- | 升目の駒
lookup :: Square -> Board -> Maybe Piece
lookup square (Board board) = Map.lookup square board

-- | 持ち駒を指せる升目リスト
drops :: Piece -> Board -> [Square]
drops piece board = filter (\square -> (lookup square board) == Nothing) $ Piece.drops piece
