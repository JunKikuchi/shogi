module Shogi
  ( Shogi
  , initShogi
  ) where

import Data.Time.Clock (UTCTime)
import Shogi.Color
import Shogi.Clock
import Shogi.Position (Position)
import Shogi.Piece    (Piece)
import Shogi.Square

-- | 将棋データ
data Shogi = Shogi
           { getStats  :: Stats  -- 状態リスト
           , getMoves  :: Moves  -- 手順リスト
           , getResult :: Result -- 結果
           } deriving (Eq, Show)

-- | 駒の状態リスト
type Stats = [Stat]

-- | 状態
data Stat = Stat
          { getStatColor    :: Color    -- 手番
          , getStatPosition :: Position -- 駒の配置
          , getStatClock    :: Clock    -- 時計
          , getStatUTCTime  :: UTCTime  -- 時間
          } deriving (Eq, Show)

-- | 手順リスト
type Moves = [Move]

-- | 手順
data Move = Move
          { getMoveColor   :: Color     -- 手番
          , getMovePiece   :: MovePiece -- 指した手
          , getMoveSec     :: Sec       -- 秒数
          , getMoveUTCTime :: UTCTime   -- 時間
          } deriving (Eq, Show)

-- | 指し手
data MovePiece = MovePiece MoveFrom MoveTo -- 駒を動かす
               | DropPiece Piece    MoveTo -- 持ち駒を指す
               | Resign                    -- 投了
               | Countdown                 -- 時計を進める
               -- | Impasse                   -- 持将棋
               deriving (Eq, Show)

-- | 結果
data Result = InProgress            -- 対局中
            | Drawn     Termination -- 引き分け
            | Win Color Termination -- 勝負がついた
            deriving (Eq, Show)

-- | 終了状態
data Termination = Checkmate      -- Win 詰み
                 | TimeForfeit    -- Win 時間切れ
                 | Resignation    -- Win 投了
                 | PerpetualCheck -- Win 連続王手の千日手
                 | Repetition     -- Drawn 千日手
                 -- | Impasse        -- Win, Drawn 持将棋
                 -- | IllegalMove    -- Win, 不正な手 (https://en.wikipedia.org/wiki/Shogi#Illegal_move)
                 deriving (Eq, Show)

-- | 初期将棋データ作成
initShogi :: Color -> Position -> Clock -> UTCTime -> Shogi
initShogi color position clock time = Shogi
                   { getStats  = Stat
                               { getStatColor   = color
                               , getPosition    = position
                               , getClock       = clock
                               , getStatUTCTime = time
                               } : []
                   , getMoves  = []
                   , getResult = InProgress
                   }

-- | 手を指す
move :: Move -> Shogi -> Maybe Shogi
move = undefined

-- | 駒を動かす手
movePiece :: Color -> MoveFrom -> MoveTo -> Move
movePiece = undefined

-- | 持ち駒を指す手
dropPiece :: Color -> Piece -> MoveTo -> Move
dropPiece = undefined

-- | 投了する手
resign :: Color -> Move
resign = undefined

-- | 時計を進める手
countdown :: Color -> Move
countdown = undefined
