module Shogi
  ( Shogi
  , shogiStats
  , shogiMoves
  , shogiResult
  , Stat
  , statColor
  , statPosition
  , statClock
  , statTime
  , Move
  , moveColor
  , moveMoveType
  , moveSec
  , moveTime
  , Result(..)
  , Termination(..)
  , initShogi
  , hirate
  , currentStat
  , countdown
  , move
  , movePiece
  , dropPiece
  , resign
  ) where

import Control.Monad (guard)
import Data.Time.Clock (UTCTime)
import qualified GameClock as GameClock
import Shogi.Color
import Shogi.Clock
import qualified Shogi.Position as Position
import qualified Shogi.Piece    as Piece
import Shogi.Position (Position)
import Shogi.Piece    (Piece)
import Shogi.Square

-- | 将棋データ
data Shogi = Shogi
           { shogiStats  :: Stats  -- 状態リスト
           , shogiMoves  :: Moves  -- 手順リスト
           , shogiResult :: Result -- 結果
           } deriving (Eq, Show)

-- | 状態リスト
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
          { moveColor    :: Color    -- 手番
          , moveMoveType :: MoveType -- 指した手
          , moveSec      :: Sec      -- 秒数
          , moveTime     :: UTCTime  -- 時間
          } deriving (Eq, Show)

-- | 指し手
data MoveType = MovePiece Square MoveTo -- 駒を動かす
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

-- | 現在の状態取得
currentStat :: Shogi -> Stat
currentStat = head . shogiStats

-- | 経過時間チェック
countdown :: Sec -> UTCTime -> Shogi -> Maybe Shogi
countdown sec time shogi = do
  guard $ shogiResult shogi == InProgress
  return $ if GameClock.over color clock
    then newShogi { shogiResult = Win (turn color) TimeForfeit }
    else newShogi
  where
    newShogi = shogi { shogiStats = stat { statClock = clock }:stats }
    clock    = GameClock.countdown sec color $ statClock stat
    color    = statColor stat
    (stat:stats) = shogiStats shogi

-- | 手を指す
move :: MoveType -> Sec -> UTCTime -> Shogi -> Maybe Shogi
move moveType sec time shogi = do
  cdShogi <- countdown sec time shogi
  if shogiResult cdShogi /= InProgress
  then return cdShogi
  else do
    let stat  = currentStat cdShogi
    let color = statColor stat
    let newMove = Move
                { moveColor    = color
                , moveMoveType = moveType
                , moveSec      = sec
                , moveTime     = time
                }
    if moveType == Resign
      then
        return $ shogi
               { shogiMoves  = newMove:shogiMoves shogi
               , shogiResult = Win (turn color) Resignation
               }
      else do
        let pos   = statPosition stat
        position <- move' moveType color pos
        let newStat = stat
                    { statColor    = turn color
                    , statPosition = position
                    , statTime     = time
                    }
        let newShogi = cdShogi
                     { shogiStats = newStat:shogiStats shogi
                     , shogiMoves = newMove:shogiMoves shogi
                     }
        return $ if Position.checkmate (turn color) position
          then newShogi { shogiResult = Win color Checkmate }
          else newShogi
  where
    move' (MovePiece square to    ) color pos = Position.move (square, color) to pos
    move' (DropPiece piece  square) color pos = if Piece.pieceColor piece == color
                                                then Position.drop piece square pos
                                                else Nothing

-- | 駒を動かす手
movePiece :: Square -> MoveTo -> MoveType
movePiece = MovePiece

-- | 持ち駒を指す手
dropPiece :: Piece -> Square -> MoveType
dropPiece = DropPiece

-- | 投了する手
resign :: MoveType
resign = Resign
