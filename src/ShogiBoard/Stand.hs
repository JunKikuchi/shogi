module ShogiBoard.Stand
  ( Stand
  , fromList
  , toList
  , pieces
  , take
  , included
  ) where

import Prelude hiding (take)
import ShogiBoard.Piece
import ShogiBoard.Color

-- | 駒台
newtype Stand = Stand [Piece] deriving (Eq, Show)

-- | 駒のリストから駒台作成
fromList :: [Piece] -> Stand
fromList = Stand

-- | 駒のリスト
toList :: Stand -> [Piece]
toList (Stand stand) = stand

-- | 手番の持ち駒リスト
pieces :: Color -> Stand -> [Piece]
pieces color (Stand stand) = filter (\piece -> getColor piece == color) stand

-- | 駒台から駒を取り除く
take :: Piece -> Color -> Stand -> Maybe (Piece, Stand)
take = undefined

-- | 駒が駒台にあるか
included :: Piece -> Stand -> Bool
included piece (Stand stand) = elem piece stand
