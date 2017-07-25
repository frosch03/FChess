{-|
Module      : Board
Description : Contains the definition of a Board including the Fields
License     : GPL-3
Maintainer  : frosch03@frosch03.de
Stability   : experimental
-}

module Data.FChess.Datatype.Board
    ( Board(..)
    , Field(..)
    , showField
    )
where

import Data.List (sortBy)

import Data.FChess.Datatype.Piece


data Field
    = Field (Int, Int) (Maybe Piece)
    deriving (Eq, Show)

showField :: Field -> [Char]
showField (Field _ (Just p)) = ' ':(show p) ++ " "
showField (Field _  Nothing) = "   "

-- | A 'Board' (for now) is just a list of 'Field's.
-- 
-- The 'Show' instance for a 'Board' generates a graphical
-- representation of the 'Board' as shown with this example:
--
-- >>> newBoard
-- ┌───┬───┬───┬───┬───┬───┬───┬───┐
-- │ ♜ │ ♞ │ ♝ │ ♛ │ ♚ │ ♝ │ ♞ │ ♜ │
-- ├───┼───┼───┼───┼───┼───┼───┼───┤
-- │ ♟ │ ♟ │ ♟ │ ♟ │ ♟ │ ♟ │ ♟ │ ♟ │
-- ├───┼───┼───┼───┼───┼───┼───┼───┤
-- │   │   │   │   │   │   │   │   │
-- ├───┼───┼───┼───┼───┼───┼───┼───┤
-- │   │   │   │   │   │   │   │   │
-- ├───┼───┼───┼───┼───┼───┼───┼───┤
-- │   │   │   │   │   │   │   │   │
-- ├───┼───┼───┼───┼───┼───┼───┼───┤
-- │   │   │   │   │   │   │   │   │
-- ├───┼───┼───┼───┼───┼───┼───┼───┤
-- │ ♙ │ ♙ │ ♙ │ ♙ │ ♙ │ ♙ │ ♙ │ ♙ │
-- ├───┼───┼───┼───┼───┼───┼───┼───┤
-- │ ♖ │ ♘ │ ♗ │ ♕ │ ♔ │ ♗ │ ♘ │ ♖ │
-- └───┴───┴───┴───┴───┴───┴───┴───┘
--
-- The 'Figure's are drawn as unicode characters. 
data Board
    = Board [Field]

-- | Making 'Board' an instance of 'Show' is done by implementing the
-- 'show' method
-- 
-- ' show' renders an initial 'spacer'. This one is then followd by an
-- alternating sequence of a row and another 'spacer'.
--
-- The row is gathered from the field by filtering out all other
-- 'Field's that are not within the given row. It is then sorted
-- by it's column.
--
-- At last it's folded to a [Char].
instance Show (Board) where
     show (Board fs)
         =    (spacer 0) ++ "\n" 
           ++ concat [showRow i ++ '\n':(spacer i) ++ "\n" | i <- enumFromThenTo 8 7 1]
         where getRow i  = filter (\(Field (x, y) _) -> x == i)
                           fs
               ordRow i  = sortBy (\(Field (x1, y1) _) (Field (x2, y2) _) -> compare x1 x2)
                           $ getRow i
               showRow i = '│':(foldl (\r n -> r ++ (showField n ++ "│")) "" $ ordRow i)
               spacerT   = "┌───┬───┬───┬───┬───┬───┬───┬───┐"
               spacerM   = "├───┼───┼───┼───┼───┼───┼───┼───┤"
               spacerB   = "└───┴───┴───┴───┴───┴───┴───┴───┘"
               spacer 0  = spacerT
               spacer 1  = spacerB
               spacer _  = spacerM

