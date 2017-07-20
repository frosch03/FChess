{-# LANGUAGE UnicodeSyntax #-}

{-|
Module      : Chess
Description : Simple Chess Module
Copyright   : (c) Matthias Brettschneider, 2017
License     : GPL-3
Maintainer  : frosch03@frosch03.de
Stability   : experimental

The Chess module is a simple haskell module, that serves myselfe as
testing ground for developing a chess engin.

This module defines the type 'Piece' as the central data type for a
chess piece.
-}

module Chess where

import Data.List (sortBy)

data Color
    = Black
    | White
    deriving (Eq, Show)

data Figure
    = Rook
    | Knight
    | Bishop
    | Queen
    | King
    | Pawn
    deriving (Eq, Show)

type Position = (Int, Int)

data Piece
    =  Piece Figure Color
    deriving (Eq)

instance Show (Piece)
    where show (Piece f c)
              | c == Black && f == Rook   = "♜"
              | c == Black && f == Knight = "♞"
              | c == Black && f == Bishop = "♝"
              | c == Black && f == Queen  = "♛"
              | c == Black && f == King   = "♚"
              | c == Black && f == Pawn   = "♟"

              | c == White && f == Rook   = "♖"
              | c == White && f == Knight = "♘"
              | c == White && f == Bishop = "♗"
              | c == White && f == Queen  = "♕"
              | c == White && f == King   = "♔"
              | c == White && f == Pawn   = "♙"

                                       
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
-- +---+---+---+---+---+---+---+---+
-- | ♖ | ♘ | ♗ | ♕ | ♔ | ♗ | ♘ | ♖ |
-- +---+---+---+---+---+---+---+---+
-- | ♙ | ♙ | ♙ | ♙ | ♙ | ♙ | ♙ | ♙ |
-- +---+---+---+---+---+---+---+---+
-- |   |   |   |   |   |   |   |   |
-- +---+---+---+---+---+---+---+---+
-- |   |   |   |   |   |   |   |   |
-- +---+---+---+---+---+---+---+---+
-- |   |   |   |   |   |   |   |   |
-- +---+---+---+---+---+---+---+---+
-- |   |   |   |   |   |   |   |   |
-- +---+---+---+---+---+---+---+---+
-- | ♟ | ♟ | ♟ | ♟ | ♟ | ♟ | ♟ | ♟ |
-- +---+---+---+---+---+---+---+---+
-- | ♜ | ♞ | ♝ | ♛ | ♚ | ♝ | ♞ | ♜ |
-- +---+---+---+---+---+---+---+---+
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
         =    spacer ++ "\n" 
           ++ concat [showRow ((8 + 1) - i) ++ '\n':spacer ++ "\n" | i <- [1..8]]
         where getRow i  = filter (\(Field (x, y) _) -> x == i)
                           fs
               ordRow i  = sortBy (\(Field (x1, y1) _) (Field (x2, y2) _) -> compare x1 x2)
                           $ getRow i
               showRow i = '|':(foldl (\r n -> r ++ (showField n ++ "|")) "" $ ordRow i)
               spacer    = "+---+---+---+---+---+---+---+---+"





-- |The 'Game' consists of a 'Board' and two lists of 'Piece's. The
-- first one contains the 'Black' 'Figure's and the second onecontains
-- the 'White' 'Figure's.
data Game
    = Game Board [Piece] [Piece]

     
newBoard :: Board
newBoard = Board
           $  whiteBaseline
           ++ whitePawnline
           ++ emptyLines
           ++ blackPawnline
           ++ blackBaseline
    where baselineFields i = map (\x -> (i, x)) [1..8]
          baselineFigures = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
          pawnlineFigures = replicate 8 Pawn
          whiteBaseline = map (\(f,p) -> Field f (Just p)) $ zip (baselineFields 1) (map (\x -> Piece x White) baselineFigures)
          whitePawnline = map (\(f,p) -> Field f (Just p)) $ zip (baselineFields 2) (map (\x -> Piece x White) pawnlineFigures)
          blackBaseline = map (\(f,p) -> Field f (Just p)) $ zip (baselineFields 8) (map (\x -> Piece x Black) baselineFigures)
          blackPawnline = map (\(f,p) -> Field f (Just p)) $ zip (baselineFields 7) (map (\x -> Piece x Black) pawnlineFigures)
          emptyLines = map (\f -> Field f Nothing) [(i,j) | i <- [3..6], j <- [1..8]]

newGame :: Game
newGame = Game newBoard [] []