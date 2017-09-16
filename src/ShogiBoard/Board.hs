module ShogiBoard.Board
  ( Board
  , fromList
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

-- | 升目と駒のリストから将棋盤作成
fromList :: [(Square, Piece)] -> Board
fromList = undefined

-- | 王手判定
check :: Color -> Board -> Bool
check = undefined

-- | 手番の駒リスト
pieces :: Color -> Board -> [(Square, Piece)]
pieces = undefined

-- | 駒を動かす
move :: MoveFrom -> MoveTo -> Color -> Board -> Maybe Board
move = undefined

-- | 持ち駒を指す
drop :: Piece -> Square -> Color -> Board -> Maybe Board
drop = undefined

-- | 駒を動かせる升目リスト
moves :: MoveFrom -> Color -> Board -> [MoveTo]
moves = undefined

-- | 持ち駒を指せる升目リスト
drops :: Piece -> Color -> Board -> [Square]
drops = undefined
