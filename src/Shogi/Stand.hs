module Shogi.Stand
  ( Stand
  , fromList
  , toList
  , pieces
  , put
  , take
  , included
  ) where

import           Data.List   (delete, sort)
import           Prelude     hiding (take)
import           Shogi.Color
import           Shogi.Piece

-- | 駒台
newtype Stand = Stand { unStand :: [Piece] } deriving (Eq, Show)

-- | 駒のリストから駒台作成
fromList :: [Piece] -> Stand
fromList = Stand . sort

-- | 駒のリスト
toList :: Stand -> [Piece]
toList (Stand stand) = stand

-- | 手番の持ち駒リスト
pieces :: Color -> Stand -> [Piece]
pieces color (Stand stand) = filter (\piece -> pieceColor piece == color) stand

-- | 駒台に駒を載せる
put :: Piece -> Stand -> Stand
put piece (Stand stand) = fromList $ piece { pieceColor = turn $ pieceColor piece, piecePromotion = False }:stand

-- | 駒台から駒を取り除く
take :: Piece -> Stand -> Maybe Stand
take piece (Stand stand)
  | piece `elem` stand = Just $ Stand $ delete piece stand
  | otherwise        = Nothing

-- | 駒が駒台にあるか
included :: Piece -> Stand -> Bool
included piece (Stand stand) = piece `elem` stand
