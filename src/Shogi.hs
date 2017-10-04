module Shogi
  ( Shogi
  , initShogi
  , hirate
  ) where

import Data.Time.Clock (UTCTime)
import Shogi.Color
import Shogi.Clock
import qualified Shogi.Position as Position
import Shogi.Position (Position)
import Shogi.Piece    (Piece)
import Shogi.Square

-- | 将棋データ
data Shogi = Shogi
           { shogiStats  :: Stats  -- 状態リスト
           , shogiMoves  :: Moves  -- 手順リスト
           , shogiResult :: Result -- 結果
           } deriving (Eq, Show)

-- | 駒の状態リスト
type Stats = [Stat]

-- | 状態
data Stat = Stat
          { statColor    :: Color    -- 手番
          , statPosition :: Position -- 駒の配置
          , statClock    :: Clock    -- 時計
          , statTime     :: UTCTime  -- 時間
          } deriving (Eq, Show)

-- | 手順リスト
type Moves = [Move]

-- | 手順
data Move = Move
          { moveColor :: Color     -- 手番
          , movePiece :: MovePiece -- 指した手
          , moveSec   :: Sec       -- 秒数
          , moveTime  :: UTCTime   -- 時間
          } deriving (Eq, Show)

-- | 指し手
data MovePiece = MovePiece MoveFrom MoveTo -- 駒を動かす
               | DropPiece Piece    MoveTo -- 持ち駒を指す
               | Resign                    -- 投了
               | TimeIsUp                  -- 時間切れ
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
initShogi color position clock time = shogi
  where
    shogi = Shogi
          { shogiStats  = [stat]
          , shogiMoves  = []
          , shogiResult = InProgress
          }
    stat = Stat
         { statColor    = color
         , statPosition = position
         , statClock    = clock
         , statTime     = time
         }

-- | 平手初期データ作成
hirate :: Clock -> UTCTime -> Shogi
hirate = initShogi Black Position.hirate

{--
-- | 経過時間チェック
countdown :: Shogi -> Sec -> UTCTime -> Maybe Shogi
countdown = undefined

-- | 手を指す
move :: Move -> Shogi -> Sec -> UTCTime -> Maybe Shogi
move = undefined

-- | 駒を動かす手
movePiece :: MoveFrom -> MoveTo -> Move
movePiece = undefined

-- | 持ち駒を指す手
dropPiece :: Piece -> MoveTo -> Move
dropPiece = undefined

-- | 投了する手
resign :: Move
resign = undefined
--}
