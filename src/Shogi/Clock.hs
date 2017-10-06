module Shogi.Clock
  ( Shogi.Clock.Clock
  , Shogi.Clock.clock
  , Sec
  ) where

import GameClock
import GameClock.Clock
import GameClock.Clock.Sec
import Shogi.Color

type Clock = GameClock Color

clock :: GameClock.Clock.Clock -> Shogi.Clock.Clock
clock = gameClock
