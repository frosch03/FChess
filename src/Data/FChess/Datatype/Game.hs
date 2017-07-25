{-|
Module      : Game
Description : Contains the game definition
License     : GPL-3
Maintainer  : frosch03@frosch03.de
Stability   : experimental
-}

module Data.FChess.Datatype.Game
    ( Game(..)
    )
where

import Data.FChess.Datatype.Piece
import Data.FChess.Datatype.Board

-- | The 'Game' consists of a 'Board' and two lists of 'Piece's. The
-- first one contains the 'Black' 'Figure's and the second onecontains
-- the 'White' 'Figure's.
data Game
    = Game Board [Piece] [Piece]


