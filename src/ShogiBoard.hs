module ShogiBoard
    (
    ) where

import qualified Data.Map as Map

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
data Shogi = Shogi Board Stand deriving (Eq, Show)

-- | 将棋盤
newtype Board = Board (Map.Map Square Piece) deriving (Eq, Show)

-- | 駒台
newtype Stand = Stand [Piece] deriving (Eq, Show)

-- | 升目
type Square = (File, Rank)

-- | 筋
data File = F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 | F9 deriving (Eq, Ord, Enum, Bounded, Show)

-- | 段
data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 deriving (Eq, Ord, Enum, Bounded, Show)

-- | 駒
data Piece = Piece PieceType Promoted Color deriving (Eq, Ord, Show)

-- | 駒の種類
data PieceType = Pawn   -- 歩兵
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

-- | 先手後手
data Color = Black -- 先手
           | White -- 後手
           deriving (Eq, Ord, Enum, Bounded, Show)

-- | 駒の移動元の升目
type MoveFrom = Square

-- | 駒の移動先の升目
type MoveTo = Square

-- | 詰み判定
checkmate :: Color -> Shogi -> Bool
checkmate = undefined

-- | 王手判定
check :: Color -> Shogi -> Bool
check = undefined

-- | 駒を動かす
move :: MoveFrom -> MoveTo -> Shogi -> Maybe Shogi
move = undefined

-- | 持ち駒を指す
drop :: Piece -> MoveTo -> Shogi -> Maybe Shogi
drop = undefined

-- | 駒を動かせる升目
moves :: MoveFrom -> Shogi -> [MoveTo]
moves = undefined

-- | 持ち駒を指せる升目
drops :: Piece -> Shogi -> [MoveTo]
drops = undefined
