{-|
Module      : Piece
Description : Contains the definition of a chess piece
License     : GPL-3
Maintainer  : frosch03@frosch03.de
Stability   : experimental
-}

module Data.FChess.Datatype.Piece
    ( Color(..)
    , Figure(..)
    , Piece(..)
    )
where

data Color
    = Black
    | White
    deriving (Eq, Show)

data Figure
    = R
    | N
    | B
    | Q
    | K
    | P
    deriving (Eq, Show)

data Piece
    =  Piece Color Figure
    deriving (Eq)

instance Show (Piece)
    where show (Piece c f)
              | c == Black && f == R = "♜"
              | c == Black && f == N = "♞"
              | c == Black && f == B = "♝"
              | c == Black && f == Q = "♛"
              | c == Black && f == K = "♚"
              | c == Black && f == P = "♟"

              | c == White && f == R = "♖"
              | c == White && f == N = "♘"
              | c == White && f == B = "♗"
              | c == White && f == Q = "♕"
              | c == White && f == K = "♔"
              | c == White && f == P = "♙"
