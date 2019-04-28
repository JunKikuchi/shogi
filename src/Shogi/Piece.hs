module Shogi.Piece
  ( Piece(..)
  , Type(..)
  , Promotion
  , pawn
  , lance
  , knight
  , silver
  , gold
  , bishop
  , rook
  , king
  , promote
  , promotions
  , moves
  , drops
  ) where

import           Shogi.Color
import           Shogi.Promotion
import           Shogi.Square

-- | 駒
data Piece = Piece
           { pieceType      :: Type
           , piecePromotion :: Promotion
           , pieceColor     :: Color
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
pawn :: Promotion -> Color -> Piece
pawn = Piece Pawn

-- | 香車
lance :: Promotion -> Color -> Piece
lance = Piece Lance

-- | 桂馬
knight :: Promotion -> Color -> Piece
knight = Piece Knight

-- | 銀将
silver :: Promotion -> Color -> Piece
silver = Piece Silver

-- | 金将
gold :: Color -> Piece
gold = Piece Gold False

-- | 角行
bishop :: Promotion -> Color -> Piece
bishop = Piece Bishop

-- | 飛車
rook :: Promotion -> Color -> Piece
rook = Piece Rook

-- | 王将
king :: Color -> Piece
king = Piece King False

-- | 成る
promote :: Piece -> Maybe Piece
promote (Piece _ True _) = Nothing
promote (Piece Gold _ _) = Nothing
promote (Piece King _ _) = Nothing
promote piece            = Just piece { piecePromotion = True }

-- | 成り不成
promotions :: Piece -> Square -> Square -> [Promotion]
promotions (Piece _      True  _    ) _         _       = [False]
promotions (Piece Pawn   False Black) _         (_, R1) = [True]
promotions (Piece Pawn   False White) _         (_, R9) = [True]
promotions (Piece Lance  False Black) _         (_, R1) = [True]
promotions (Piece Lance  False White) _         (_, R9) = [True]
promotions (Piece Knight False Black) _         (_, R1) = [True]
promotions (Piece Knight False Black) _         (_, R2) = [True]
promotions (Piece Knight False White) _         (_, R8) = [True]
promotions (Piece Knight False White) _         (_, R9) = [True]
promotions (Piece Gold   False _    ) _         _       = [False]
promotions (Piece King   False _    ) _         _       = [False]
promotions (Piece _      False Black) (_, from) (_, to) = if from <= R3 || to <= R3 then [False, True] else [False]
promotions (Piece _      False White) (_, from) (_, to) = if from >= R7 || to >= R7 then [False, True] else [False]

-- | 駒が動ける升目
moves :: Piece -> Square -> [[Square]]
moves (Piece Pawn   False Black) square = map (one . ($ square)) [upper ]
moves (Piece Pawn   False White) square = map (one . ($ square)) [bottom]
moves (Piece Pawn   True  color) square = moves (gold color) square

moves (Piece Lance  False Black) square = map ($ square) [upper]
moves (Piece Lance  False White) square = map ($ square) [bottom]
moves (Piece Lance  True  color) square = moves (gold color) square

moves (Piece Knight False Black) (file, rank)
  | rank <= R2 = undefined
  | file == F1 = [left']
  | file == F9 = [right']
  | otherwise  = [left', right']
  where
    left'  = [(succ file, pred . pred $ rank)]
    right' = [(pred file, pred . pred $ rank)]
moves (Piece Knight False White) (file, rank)
  | rank >= R8 = undefined
  | file == F1 = [left']
  | file == F9 = [right']
  | otherwise  = [left', right']
  where
    left'  = [(succ file, succ . succ $ rank)]
    right' = [(pred file, succ . succ $ rank)]
moves (Piece Knight True  color) square = moves (gold color) square

moves (Piece Silver False Black) square = map (one . ($ square)) [upperLeft,  upper,  upperRight,  bottomRight, bottomLeft]
moves (Piece Silver False White) square = map (one . ($ square)) [bottomLeft, bottom, bottomRight, upperRight,  upperLeft ]
moves (Piece Silver True  color) square = moves (gold color) square

moves (Piece Gold   False Black) square = map (one . ($ square)) [left, upperLeft,  upper,  upperRight,  right, bottom]
moves (Piece Gold   False White) square = map (one . ($ square)) [left, bottomLeft, bottom, bottomRight, right, upper ]
moves (Piece Gold   True  _    ) square = undefined

moves (Piece Bishop False _    ) square = map ($ square) [            upperLeft,              upperRight,              bottomRight,               bottomLeft]
moves (Piece Bishop True  _    ) square = map ($ square) [one . left, upperLeft, one . upper, upperRight, one . right, bottomRight, one . bottom, bottomLeft]

moves (Piece Rook   False _    ) square = map ($ square) [left,                  upper,                   right,                    bottom                 ]
moves (Piece Rook   True  _    ) square = map ($ square) [left, one . upperLeft, upper, one . upperRight, right, one . bottomRight, bottom, one .bottomLeft]

moves (Piece King   False _    ) square = map (one . ($ square)) [left, upperLeft, upper, upperRight, right, bottomRight, bottom, bottomLeft]
moves (Piece King   True  _    ) square = undefined

-- | ひとつの升目を動かす
one :: [Square] -> [Square]
one = take 1

-- | 左の動き
left :: Square -> [Square]
left (file, rank) = [(file', rank) | file' <- leftFiles file]

-- | 左上の動き
upperLeft :: Square -> [Square]
upperLeft (file, rank) = zip (leftFiles file) (upperRanks rank)

-- | 上の動き
upper :: Square -> [Square]
upper (_,    R1  ) = []
upper (file, rank) = [(file, rank') | rank' <- upperRanks rank]

-- | 右上の動き
upperRight :: Square -> [Square]
upperRight (file, rank) = zip (rightFiles file) (upperRanks rank)

-- | 右の動き
right :: Square -> [Square]
right (file, rank) = [(file', rank) | file' <- rightFiles file]

-- | 右下の動き
bottomRight :: Square -> [Square]
bottomRight (file, rank) = zip (rightFiles file) (bottomRanks rank)

-- | 下の動き
bottom :: Square -> [Square]
bottom (file, rank) = [(file, rank') | rank' <- bottomRanks rank]

-- | 左下の動き
bottomLeft :: Square -> [Square]
bottomLeft (file, rank) = zip (leftFiles file) (bottomRanks rank)

-- | 左向きの段
leftFiles F9   = []
leftFiles file = drop 1 $ enumFrom file

-- | 右向きの段
rightFiles F1   = []
rightFiles file = drop 1 $ enumFromThen file $ pred file

-- | 上向きの筋
upperRanks R1   = []
upperRanks rank = drop 1 $ enumFromThen rank $ pred rank

-- | 下向きの筋
bottomRanks R9   = []
bottomRanks rank = drop 1 $ enumFrom rank

-- | 駒を打てる升目
drops :: Piece -> [Square]
drops (Piece Pawn   _ Black) = [(file, rank) | file <- [F1 .. F9], rank <- [R2 .. R9]]
drops (Piece Pawn   _ White) = [(file, rank) | file <- [F1 .. F9], rank <- [R1 .. R8]]
drops (Piece Lance  _ Black) = [(file, rank) | file <- [F1 .. F9], rank <- [R2 .. R9]]
drops (Piece Lance  _ White) = [(file, rank) | file <- [F1 .. F9], rank <- [R1 .. R8]]
drops (Piece Knight _ Black) = [(file, rank) | file <- [F1 .. F9], rank <- [R3 .. R9]]
drops (Piece Knight _ White) = [(file, rank) | file <- [F1 .. F9], rank <- [R1 .. R7]]
drops _                      = [(file, rank) | file <- [F1 .. F9], rank <- [R1 .. R9]]
