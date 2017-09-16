module ShogiBoard.Color
  ( Color(..)
  , turn
  ) where

-- | 先手後手
data Color = Black -- 先手
           | White -- 後手
           deriving (Eq, Ord, Enum, Bounded, Show)

-- | 手番変更
turn :: Color -> Color
turn Black = White
turn _     = Black
