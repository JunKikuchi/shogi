module ShogiBoard.Piece
  ( Piece
  , getType
  , getPromoted
  , getColor
  , Promoted
  , pawn
  , lance
  , knight
  , silver
  , gold
  , bishop
  , rook
  , king
  , promote
  , moves
  ) where

import ShogiBoard.Color
import ShogiBoard.Promoted
import ShogiBoard.Square

-- | 駒
data Piece = Piece
           { getType     :: Type
           , getPromoted :: Promoted
           , getColor    :: Color
           } deriving (Eq, Ord, Show)

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

-- | 歩兵
pawn :: Promoted -> Color -> Piece
pawn = Piece Pawn

-- | 香車
lance :: Promoted -> Color -> Piece
lance = Piece Lance

-- | 桂馬
knight :: Promoted -> Color -> Piece
knight = Piece Knight

-- | 銀将
silver :: Promoted -> Color -> Piece
silver = Piece Silver

-- | 金将
gold :: Color -> Piece
gold = Piece Gold False

-- | 角行
bishop :: Promoted -> Color -> Piece
bishop = Piece Bishop

-- | 飛車
rook :: Promoted -> Color -> Piece
rook = Piece Rook

-- | 王将
king :: Color -> Piece
king = Piece Gold False

-- | 成る
promote :: Piece -> Maybe Piece
promote (Piece _ True _) = Nothing
promote (Piece Gold _ _) = Nothing
promote (Piece King _ _) = Nothing
promote piece = Just piece { getPromoted = True }

-- | 駒が動ける升目
moves :: Piece -> Square -> [[Square]]
moves = undefined
