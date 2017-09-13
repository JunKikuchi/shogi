module ShogiBoard.Piece
  ( Piece
  , Promoted
  ) where

import ShogiBoard.Color

-- | 駒
data Piece = Piece Type Promoted Color deriving (Eq, Ord, Show)

-- | 駒の種類
data Type = Pawn   -- 歩兵
               | Lance  -- 香車
               | Knight -- 桂馬
               | Silver -- 銀将
               | Gold   -- 金将
               | Bishop -- 角行
               | Rook   -- 飛車
               | King   -- 王将
               deriving (Eq, Ord, Enum, Bounded, Show)

-- | 成駒
type Promoted = Bool
