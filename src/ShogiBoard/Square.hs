module ShogiBoard.Square
  ( Square
  , File(..)
  , Rank(..)
  , MoveFrom
  , MoveTo
  ) where

import ShogiBoard.Color
import ShogiBoard.Promoted

-- | 升目
type Square = (File, Rank)

-- | 筋
data File = F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 | F9 deriving (Eq, Ord, Enum, Bounded, Show)

-- | 段
data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 deriving (Eq, Ord, Enum, Bounded, Show)

-- | 駒の移動元の升目
type MoveFrom = Square

-- | 駒の移動先の升目
type MoveTo = (Square, Promoted)
