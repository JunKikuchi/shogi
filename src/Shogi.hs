module Shogi
  ( Shogi
  , shogiStat
  , shogiMoves
  , shogiResult
  , Stat
  , statColor
  , statPosition
  , statClock
  , statTime
  , Move(..)
  , MoveType(..)
  , Result(..)
  , Termination(..)
  , shogi
  , hirate
  , countdown
  , move
  , movePiece
  , dropPiece
  , resign
  ) where

import Control.Monad (guard)
import Data.Time.Clock (UTCTime)
import qualified GameClock
import Shogi.Color
import Shogi.Clock
import qualified Shogi.Position as Position
import qualified Shogi.Piece    as Piece
import Shogi.Position (Position)
import Shogi.Piece    (Piece)
import Shogi.Square

-- | 将棋データ
data Shogi = Shogi
           { shogiStat   :: Stat   -- 最新の局面
           , shogiMoves  :: Moves  -- 手順リスト
           , shogiResult :: Result -- 結果
           } deriving (Eq, Show)

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
          { moveColor :: Color    -- 手番
          , moveType  :: MoveType -- 指した手
          , moveSec   :: Sec      -- 秒数
          , moveTime  :: UTCTime  -- 時間
          , moveStat  :: Stat     -- 指した後の局面
          } deriving (Eq, Show)

-- | 指し手
data MoveType = Init                    -- 初期配置
              | MovePiece Square MoveTo -- 駒を動かす
              | DropPiece Piece  Square -- 持ち駒を指す
              | Resign                  -- 投了
              | TimeIsUp                -- 時間切れ
              -- | Impasse                 -- 持将棋
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

-- | 将棋データ作成
shogi :: Color -> Position -> Clock -> UTCTime -> Shogi
shogi color position clock time = shogi'
  where
    shogi' = Shogi
           { shogiStat   = stat
           , shogiMoves  = [move]
           , shogiResult = InProgress
           }
    stat = Stat
         { statColor    = color
         , statPosition = position
         , statClock    = clock
         , statTime     = time
         }
    move = Move
         { moveColor = color
         , moveType  = Init
         , moveSec   = 0
         , moveTime  = time
         , moveStat  = stat
         }

-- | 平手初期データ作成
hirate :: Clock -> UTCTime -> Shogi
hirate = shogi Black Position.hirate

-- | 経過時間チェック
countdown :: Sec -> UTCTime -> Shogi -> Maybe Shogi
countdown sec time shogi = do
  guard $ shogiResult shogi == InProgress
  return $ if GameClock.over color clock'
    then shogi' { shogiMoves = move':moves, shogiResult = result' }
    else shogi'
  where
    color  = statColor stat
    stat   = shogiStat shogi
    moves  = shogiMoves shogi
    shogi' = shogi { shogiStat = stat' }
    stat'  = stat { statClock = clock', statTime = time }
    clock' = GameClock.countdown sec color $ statClock stat
    move'  = Move
           { moveColor = color
           , moveType  = TimeIsUp
           , moveSec   = sec
           , moveTime  = time
           , moveStat  = stat'
           }
    result' = Win (turn color) TimeForfeit

-- | 手を指す
move :: MoveType -> Sec -> UTCTime -> Shogi -> Maybe Shogi
move moveType sec time shogi = undefined

-- | 駒を動かす手
movePiece :: Square -> MoveTo -> MoveType
movePiece = MovePiece

-- | 持ち駒を指す手
dropPiece :: Piece -> Square -> MoveType
dropPiece = DropPiece

-- | 投了する手
resign :: MoveType
resign = Resign
