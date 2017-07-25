{-|
Module      : FChess
Description : Here is the frogchess mechanic
License     : GPL-3
Maintainer  : frosch03@frosch03.de
Stability   : experimental
-}

module Data.FChess.FChess
    ( newGame
    )
where

import Data.FChess.Defaults
import Data.FChess.Mechanic
import Data.FChess.Datatype.Game

newGame :: Game
newGame = Game defaultBoard [] []

