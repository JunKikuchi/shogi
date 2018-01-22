{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Shogi.Color
  ( Color(..)
  , turn
  ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Text        (pack, unpack)

-- | 先手後手
data Color = Black -- 先手
           | White -- 後手
           deriving (Eq, Ord, Enum, Bounded, Show)

$(deriveJSON defaultOptions ''Color)

instance ToJSONKey Color where
  toJSONKey = toJSONKeyText $ pack . show

instance FromJSONKey Color where
  fromJSONKey = FromJSONKeyTextParser convert
    where
      convert "Black" = pure Black
      convert "White" = pure White
      convert t       = fail $ "Cannot parse key into Color: " ++ unpack t

-- | 手番変更
turn :: Color -> Color
turn Black = White
turn _     = Black
