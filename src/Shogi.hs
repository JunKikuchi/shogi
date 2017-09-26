module Shogi
  ( Shogi
  , shogi
  , move
  , checkmate
  , check
  ) where

import Data.Time.Clock (UTCTime)
import Shogi.Color
import Shogi.Clock
import Shogi.Position (Position)
import Shogi.Piece    (Piece)
import Shogi.Square

-- | 将棋データ
data Shogi = Shogi
           { getInitPosition :: Position -- 駒の初期状態
           , getMoves        :: Moves    -- 手順リスト
           , getResult       :: Result   -- 結果
           } deriving (Eq, Show)

-- | 手順リスト
type Moves = [Move]

-- | 手順
data Move = Move
          { getColor     :: Color     -- 手番
          , getMovePiece :: MovePiece -- 指した手
          , getSec       :: Sec       -- 秒数
          , getUTCTime   :: UTCTime   -- 時間
          , getPosition  :: Position  -- 局面
          , getClock     :: Clock     -- 時計
          } deriving (Eq, Show)

-- | 指した手
data MovePiece = MovePiece MoveFrom MoveTo -- 駒を動かす
               | DropPiece Piece    MoveTo -- 持ち駒を指す
               | Resign                    -- 投了
               | Clock                     -- 時計を進める
               -- | Impasse                   -- 持将棋
               deriving (Eq, Show)

-- | 結果
data Result = InProgress                -- 対局中
            | Drawn     DrawTermination -- 引き分け
            | Win Color WinTermination  -- 勝負がついた
            deriving (Eq, Show)

-- | 引き分けの場合の終了状態
data DrawTermination = Repetition     -- 千日手 (     Drawn)
                     -- | Impasse        -- 持将棋 (Win, Drawn)
                     deriving (Eq, Show)

-- | 勝負がついた場合の終了状態
data WinTermination = Checkmate      -- 詰み
                    | TimeForfeit    -- 時間切れ
                    | Resignation    -- 投了
                    | PerpetualCheck -- 連続王手の千日手
                    -- | Impasse        -- 持将棋 (Win, Drawn)
                    -- | IllegalMove    -- 不正な手 (https://en.wikipedia.org/wiki/Shogi#Illegal_move)
                    deriving (Eq, Show)

shogi :: Position -> Moves -> Result -> Shogi
shogi = undefined

checkmate :: Color -> Shogi -> Bool
checkmate = undefined

check :: Color -> Shogi -> Bool
check = undefined

move :: MovePiece -> Shogi -> Maybe Shogi
move = undefined
