module Shogi
  ( Shogi
  , shogi
  , move
  , checkmate
  , check
  ) where

-- import Data.Time.Clock (UTCTime)
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
type Moves = [MoveA]

-- | 手順
data MoveA = MoveA
            { getColor    :: Color    -- 手番
            , getMoveType :: MoveType -- 指した手
            , getSec      :: Sec      -- 秒数
  --          , getUTCTime  :: UTCTime  -- 時間
            , getPosition :: Position -- 局面
            , getClock    :: Clock    -- 時計
            } deriving (Eq, Show)

-- | 指した手
data MoveType = Move MoveFrom MoveTo -- 駒を動かす
              | Drop Piece    MoveTo -- 持ち駒を指す
              | ResignA               -- 投了
              | ImpasseA              -- 持将棋
              | Clock                -- 時計を進める
              deriving (Eq, Show)

-- | 結果
data Result = InProgress            -- 対局中
            | Drawn     Termination -- 引き分け
            | Win Color Termination -- 勝負がついた
            deriving (Eq, Show)

-- | 終了状態
data Termination = Checkmate         -- 詰み
                 | TimeForfeit       -- 時間切れ
                 | Resign            -- 投了
                 | PerpetualCheck    -- 連続王手の千日手
                 | IllegalMove       -- 不正な手を指した
                 | RepetitionOfMoves -- 千日手 (     Drawn)
                 | Impasse           -- 持将棋 (Win, Drawn)
                 deriving (Eq, Show)

shogi :: Position -> Moves -> Result -> Shogi
shogi = undefined

getLastPosition :: Shogi -> Position
getLastPosition = undefined

checkmate :: Color -> Shogi -> Bool
checkmate = undefined

check :: Color -> Shogi -> Bool
check = undefined

move :: MoveA -> Shogi -> Maybe Shogi
move = undefined
