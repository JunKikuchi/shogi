{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Shogi
  ( Shogi
  , shogiState
  , shogiMoves
  , shogiResult
  , State
  , stateColor
  , statePosition
  , stateClock
  , stateTime
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

import           Control.Monad   (guard)
import qualified Data.Aeson      as Aeson
import           Data.Aeson.TH
import           Data.Time.Clock (UTCTime)
import qualified GameClock
import           Shogi.Clock
import           Shogi.Color
import           Shogi.Piece     (Piece)
import qualified Shogi.Piece     as Piece
import           Shogi.Position  (Position)
import qualified Shogi.Position  as Position
import           Shogi.Square

-- | 将棋データ
data Shogi = Shogi
           { shogiState  :: State  -- 最新の局面
           , shogiMoves  :: Moves  -- 手順リスト
           , shogiResult :: Result -- 結果
           } deriving (Eq, Show)

-- | 状態
data State = State
           { stateColor    :: Color    -- 手番
           , statePosition :: Position -- 駒の配置
           , stateClock    :: Clock    -- 時計
           , stateTime     :: UTCTime  -- 時間
           } deriving (Eq, Show)

-- | 手順リスト
type Moves = [Move]

-- | 手順
data Move = Move
          { moveColor :: Color    -- 手番
          , moveType  :: MoveType -- 指した手
          , moveSec   :: Sec      -- 秒数
          , moveTime  :: UTCTime  -- 時間
          , moveState :: State    -- 指した後の局面
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

$(deriveJSON defaultOptions ''Shogi)
$(deriveJSON defaultOptions ''State)
$(deriveJSON defaultOptions ''Move)
$(deriveJSON defaultOptions ''MoveType)
$(deriveJSON defaultOptions ''Result)
$(deriveJSON defaultOptions ''Termination)

-- | 将棋データ作成
shogi :: Color -> Position -> Clock -> UTCTime -> Shogi
shogi color position clock time = shogi'
  where
    shogi' = Shogi
           { shogiState  = state
           , shogiMoves  = [move]
           , shogiResult = InProgress
           }
    state = State
          { stateColor    = color
          , statePosition = position
          , stateClock    = clock
          , stateTime     = time
          }
    move = Move
         { moveColor = color
         , moveType  = Init
         , moveSec   = 0
         , moveTime  = time
         , moveState = state
         }

-- | 平手初期データ作成
hirate :: Clock -> UTCTime -> Shogi
hirate = shogi Black Position.hirate

-- | 経過時間チェック
countdown :: Sec -> UTCTime -> Shogi -> Maybe Shogi
countdown sec time shogi = do
  guard $ shogiResult shogi == InProgress
  return $ if GameClock.over color clock'
    then shogi' { shogiMoves  = move':moves
                , shogiResult = result'
                }
    else shogi'
  where
    color  = stateColor state
    state  = shogiState shogi
    moves  = shogiMoves shogi
    shogi' = shogi { shogiState = state' }
    state' = state { stateClock = clock', stateTime = time }
    clock' = GameClock.countdown sec color $ stateClock state
    move'  = Move
           { moveColor = color
           , moveType  = TimeIsUp
           , moveSec   = sec
           , moveTime  = time
           , moveState = state'
           }
    result' = Win (turn color) TimeForfeit

-- | 手を指す
move :: MoveType -> Sec -> UTCTime -> Shogi -> Maybe Shogi
move moveType sec time shogi = do
  shogi' <- countdown sec time shogi
  if shogiResult shogi' /= InProgress
    then return shogi'
    else do
      let state = shogiState shogi'
      let color = stateColor state
      let move' = Move
                { moveColor = color
                , moveType  = moveType
                , moveSec   = sec
                , moveTime  = time
                , moveState = state
                }
      if moveType == Resign
        then return $ shogi' { shogiMoves  = move':shogiMoves shogi'
                             , shogiResult = Win (turn color) Resignation
                             }
        else do
          position <- movePosition moveType color (statePosition state)
          let newState = state
                       { stateColor    = turn color
                       , statePosition = position
                       , stateTime     = time
                       }
          let newMove = move' { moveState = newState }
          let newShogi = shogi'
                       { shogiState = newState
                       , shogiMoves = newMove:shogiMoves shogi'
                       }
          return $ if Position.checkmate (turn color) position
            then newShogi { shogiResult = Win color Checkmate }
            else newShogi
  where
    movePosition (MovePiece square moveTo) color position = Position.move (square, color) moveTo position
    movePosition (DropPiece piece square ) color position = do
      guard $ color == Piece.pieceColor piece
      Position.drop piece square position

-- | 駒を動かす手
movePiece :: Square -> MoveTo -> MoveType
movePiece = MovePiece

-- | 持ち駒を指す手
dropPiece :: Piece -> Square -> MoveType
dropPiece = DropPiece

-- | 投了する手
resign :: MoveType
resign = Resign
