module ShogiBoard.Stand
  ( Stand
  , fromList
  , toList
  , pieces
  , put
  , take
  , included
  ) where

import Prelude hiding (take)
import Data.List (delete)
import ShogiBoard.Piece
import ShogiBoard.Color

-- | 駒台
newtype Stand = Stand [Piece] deriving (Eq, Show)

-- | 駒のリストから駒台作成
fromList :: [Piece] -> Stand
fromList = Stand

-- | 駒のリスト
toList :: Stand -> [Piece]
toList (Stand stand) = stand

-- | 手番の持ち駒リスト
pieces :: Color -> Stand -> [Piece]
pieces color (Stand stand) = filter (\piece -> getColor piece == color) stand

-- | 駒台に駒を載せる
put :: Piece -> Stand -> Stand
put piece (Stand stand) = Stand $ piece { getColor = turn $ getColor piece }:stand

-- | 駒台から駒を取り除く
take :: Piece -> Stand -> Maybe Stand
take piece (Stand stand)
  | elem piece stand = Just $ Stand $ delete piece stand
  | otherwise        = Nothing

-- | 駒が駒台にあるか
included :: Piece -> Stand -> Bool
included piece (Stand stand) = elem piece stand
