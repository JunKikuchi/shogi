module Shogi.Clock (Clock, Sec) where

import GameClock.Clock.Sec
import GameClock
import Shogi.Color

type Clock = GameClock Color
