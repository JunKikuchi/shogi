module ShogiBoard.Color (Color) where 

-- | 先手後手
data Color = Black -- 先手
           | White -- 後手
           deriving (Eq, Ord, Enum, Bounded, Show)
